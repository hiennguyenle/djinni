/**
 * Copyright 2014 Dropbox, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package djinni

import djinni.ast.Record.DerivingType
import djinni.ast._
import djinni.generatorTools._
import djinni.meta._
import djinni.writer.IndentWriter

import scala.collection.mutable

class CppGenerator(spec: Spec) extends Generator(spec) {

  val marshal = new CppMarshal(spec)

  val writeCppFile = writeCppFileGeneric(spec.cppOutFolder.get, spec.cppNamespace, spec.cppFileIdentStyle, spec.cppIncludePrefix) _

  private def writeHppFile(name: String, origin: String, includes: Iterable[String], fwds: Iterable[String], f: IndentWriter => Unit, f2: IndentWriter => Unit = (w => {})): Unit =
    writeHppFileGeneric(spec.cppHeaderOutFolder.get, spec.cppNamespace, spec.cppFileIdentStyle)(name, origin, includes, fwds, f, f2)

  class CppRefs(name: String) {
    var hpp = mutable.TreeSet[String]()
    var hppFwds = mutable.TreeSet[String]()
    var cpp = mutable.TreeSet[String]()

    def find(ty: TypeRef, forwardDeclareOnly: Boolean) {
      find(ty.resolved, forwardDeclareOnly)
    }

    def find(tm: MExpr, forwardDeclareOnly: Boolean) {
      tm.args.foreach((x) => find(x, forwardDeclareOnly))
      find(tm.base, forwardDeclareOnly)
    }

    def find(m: Meta, forwardDeclareOnly: Boolean) = {
      for (r <- marshal.hppReferences(m, name, forwardDeclareOnly)) r match {
        case ImportRef(arg) => hpp.add("#include " + arg)
        case DeclRef(decl, Some(spec.cppNamespace)) => hppFwds.add(decl)
        case DeclRef(_, _) =>
      }

      for (r <- marshal.cppReferences(m, name, forwardDeclareOnly)) r match {
        case ImportRef(arg) => cpp.add("#include " + arg)
        case DeclRef(_, _) =>
      }
    }
  }

  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum, deprecated: scala.Option[Deprecated]) {
    val refs = new CppRefs(ident.name)
    val self = marshal.typename(ident, e)

    if (spec.cppEnumHashWorkaround) {
      refs.hpp.add("#include <functional>") // needed for std::hash
    }

    val flagsType = "unsigned"
    val enumType = "int"
    val underlyingType = if (e.flags) flagsType else enumType

    writeHppFile(ident, origin, refs.hpp, refs.hppFwds, w => {
      w.w("enum class")
      marshal.deprecatedAnnotation(deprecated).foreach(w.w)
      w.w(s" $self : $underlyingType").bracedSemi {
        writeEnumOptionNone(w, e, idCpp.enum, marshal)
        writeEnumOptions(w, e, idCpp.enum, marshal)
        writeEnumOptionAll(w, e, idCpp.enum, marshal)
      }

      if (e.flags) {
        // Define some operators to make working with "enum class" flags actually practical
        def binaryOp(op: String) {
          w.w(s"constexpr $self operator$op($self lhs, $self rhs) noexcept").braced {
            w.wl(s"return static_cast<$self>(static_cast<$flagsType>(lhs) $op static_cast<$flagsType>(rhs));")
          }
          w.w(s"inline $self& operator$op=($self& lhs, $self rhs) noexcept").braced {
            w.wl(s"return lhs = lhs $op rhs;") // Ugly, yes, but complies with C++11 restricted constexpr
          }
        }

        binaryOp("|")
        binaryOp("&")
        binaryOp("^")

        w.w(s"constexpr $self operator~($self x) noexcept").braced {
          w.wl(s"return static_cast<$self>(~static_cast<$flagsType>(x));")
        }
      }
    },
      w => {
        // std::hash specialization has to go *outside* of the wrapNs
        if (spec.cppEnumHashWorkaround) {
          val fqSelf = marshal.fqTypename(ident, e)
          w.wl
          wrapNamespace(w, "std",
            (w: IndentWriter) => {
              w.wl("template <>")
              w.w(s"struct hash<$fqSelf>").bracedSemi {
                w.w(s"size_t operator()($fqSelf type) const").braced {
                  w.wl(s"return std::hash<$underlyingType>()(static_cast<$underlyingType>(type));")
                }
              }
            }
          )
        }
      })
  }

  def shouldConstexpr(c: Const): Boolean = {
    // Make sure we don't constexpr optionals as some might not support it
    val canConstexpr = c.ty.resolved.base match {
      case p: MPrimitive if c.ty.resolved.base != MOptional => true
      case _ => false
    }
    canConstexpr
  }

  def generateHppConstants(w: IndentWriter, consts: Seq[Const]): Unit = {
    for (c <- consts) {
      // set value in header if can constexpr (only primitives)
      var constexpr = shouldConstexpr(c)
      var constValue = ";"
      if (constexpr) {
        constValue = c.value match {
          case l: Long => " = " + l.toString + ";"
          case d: Double if marshal.fieldType(c.ty) == "float" => " = " + d.toString + "f;"
          case d: Double => " = " + d.toString + ";"
          case b: Boolean => if (b) " = true;" else " = false;"
          case _ => ";"
        }
      }
      val constFieldType = if (constexpr) s"constexpr ${marshal.fieldType(c.ty)}" else s"${marshal.fieldType(c.ty)} const"
      // Write code to the header file
      w.wl
      writeDoc(w, c.doc)
      w.wl(s"static $constFieldType ${idCpp.const(c.ident)}${constValue}")
    }
  }

  def generateCppConstants(w: IndentWriter, consts: Seq[Const], selfName: String): Unit = {
    def writeCppConst(w: IndentWriter, ty: TypeRef, v: Any): Unit = v match {
      case l: Long => w.w(l.toString)
      case d: Double if marshal.fieldType(ty) == "float" => w.w(d.toString + "f")
      case d: Double => w.w(d.toString)
      case b: Boolean => w.w(if (b) "true" else "false")
      case s: String => w.w("{" + s + "}")
      case e: EnumValue => w.w(marshal.typename(ty) + "::" + idCpp.enum(e.name))
      case v: ConstRef => w.w(selfName + "::" + idCpp.const(v))
      case z: Map[_, _] => // Value is record
        val recordMdef = ty.resolved.base.asInstanceOf[MDef]
        val record = recordMdef.body.asInstanceOf[Record]
        val vMap = z.asInstanceOf[Map[String, Any]]
        w.wl(marshal.typename(ty) + "(")
        w.increase()
        // Use exact sequence
        val skipFirst = SkipFirst()
        for (f <- record.fields) {
          skipFirst {
            w.wl(",")
          }
          writeCppConst(w, f.ty, vMap.apply(f.ident.name))
          w.w(" /* " + idCpp.field(f.ident) + " */ ")
        }
        w.w(")")
        w.decrease()
    }

    val skipFirst = SkipFirst()
    for (c <- consts) {
      skipFirst {
        w.wl
      }
      if (shouldConstexpr(c)) {
        w.w(s"${marshal.fieldType(c.ty)} constexpr $selfName::${idCpp.const(c.ident)}")
      } else {
        w.w(s"${marshal.fieldType(c.ty)} const $selfName::${idCpp.const(c.ident)} = ")
        writeCppConst(w, c.ty, c.value)
      }
      w.wl(";")
    }
  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record, deprecated: scala.Option[Deprecated], idl: Seq[TypeDecl]) {
    val refs = new CppRefs(ident.name)
    r.fields.foreach(f => refs.find(f.ty, forwardDeclareOnly = false))
    r.consts.foreach(c => refs.find(c.ty, forwardDeclareOnly = false))

    refs.hpp.add("#include <utility>") // Add for std::move
    refs.hpp.add("#include <string>") // Add for std::string
//    refs.hpp.add("#include <json.hpp>")
//    refs.hpp.add("#include <json+extension.hpp>")

    val self = marshal.typename(ident, r)
    val (cppName, cppFinal) = if (r.ext.cpp) (ident.name + "_base", "") else (ident.name, "")

    val actualSelf = marshal.typename(cppName, r)
    // Requiring the extended class
    if (r.ext.cpp) {
      refs.cpp.add("#include " + q(spec.cppExtendedRecordIncludePrefix + spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt))
    }

    refs.cpp.add("#include <sstream>")

    val superRecord = getSuperRecord(idl, r)

    superRecord match {
      case None =>
      case Some(value) =>
        refs.hpp.add("#include " + q(spec.cppExtendedRecordIncludePrefix + spec.cppFileIdentStyle(value.ident + "." + spec.cppHeaderExt)))
    }

    val superFields: Seq[Field] = superRecord match {
      case None => Seq.empty
      case Some(value) => value.fields
    }

    val fields = superFields ++ r.fields

    // C++ Header
    def writeCppPrototype(w: IndentWriter) {
      if (r.ext.cpp) {
        w.w(s"struct $self; // Requiring extended class")
        w.wl
        w.wl
      }

      writeDoc(w, doc)

      writeCppTypeParams(w, params)
      w.w("struct ")
      marshal.deprecatedAnnotation(deprecated).foreach(w.w)
      w.w(" " + actualSelf + marshal.extendsRecord(idl, r) + cppFinal).bracedSemi {
        generateHppConstants(w, r.consts)
        // Field definitions.
        for (f <- r.fields) {
          writeDoc(w, f.doc)
          w.wl(marshal.fieldType(f.ty) + " " + idCpp.field(f.ident) + ";")
        }

        if (r.derivingTypes.contains(DerivingType.Eq)) {
          w.wl
          w.wl(s"friend bool operator==(const $actualSelf& lhs, const $actualSelf& rhs);")
          w.wl(s"friend bool operator!=(const $actualSelf& lhs, const $actualSelf& rhs);")
        }
        if (r.derivingTypes.contains(DerivingType.Ord)) {
          w.wl
          w.wl(s"friend bool operator<(const $actualSelf& lhs, const $actualSelf& rhs);")
          w.wl(s"friend bool operator>(const $actualSelf& lhs, const $actualSelf& rhs);")
        }
        if (r.derivingTypes.contains(DerivingType.Eq) && r.derivingTypes.contains(DerivingType.Ord)) {
          w.wl
          w.wl(s"friend bool operator<=(const $actualSelf& lhs, const $actualSelf& rhs);")
          w.wl(s"friend bool operator>=(const $actualSelf& lhs, const $actualSelf& rhs);")
        }

        // Constructor.
        if (r.fields.nonEmpty) {
          w.wl
          writeAlignedCall(w, actualSelf + "(", superFields ++ r.fields, ")", f => marshal.fieldType(f.ty) + " " + idCpp.local(f.ident) + "_")
          w.wl
          val init = (f: Field) => idCpp.field(f.ident) + "(std::move(" + idCpp.local(f.ident) + "_))"

          superRecord match {
            case None => w.wl(": " + init(r.fields.head))
            case Some(value) =>
              w.wl(": ")
              val superRecordName = marshal.typename(value.ident, value.record)
              writeAlignedCall(w, superRecordName + "(", superFields, ")", f => " " + idCpp.local(f.ident) + "_")
              w.w(", " + init(r.fields.head))
          }

          r.fields.tail.map(f => ", " + init(f)).foreach(w.wl)
          w.wl("{}")

          w.wl
          w.w(s"$actualSelf() = default;")
          w.wl
        }

        if (r.ext.cpp) {
          w.wl
          w.wl(s"virtual ~$actualSelf() = default;")
          w.wl
          // Defining the dtor disables implicit copy/move operation generation, so re-enable them
          // Make them protected to avoid slicing
          w.wlOutdent("protected:")
          w.wl(s"$actualSelf(const $actualSelf&) = default;")
          w.wl(s"$actualSelf($actualSelf&&) = default;")
          w.wl(s"$actualSelf& operator=(const $actualSelf&) = default;")
          w.wl(s"$actualSelf& operator=($actualSelf&&) = default;")
        }

//        superRecord match {
//          case None => w.wl("virtual std::string description() const;")
//          case Some(value) => w.wl("std::string description() const override;")
//        }
      }
    }

    def writeJsonExtension(w: IndentWriter) {
      w.wl
      val recordSelf = marshal.fqTypename(ident, r)
      w.w("namespace nlohmann").braced {
        w.wl("template <>")
        w.w(s"struct adl_serializer<${recordSelf}> ").braced {
          // From JSON
          w.w(s"static $recordSelf from_json(const json & j) ").braced {
            w.wl(s"auto result = ${recordSelf}();")
            for (i <- fields.indices) {
              val name = idCpp.field(fields(i).ident)
              fields(i).ty.resolved.base match {
                case df: MDef => df.defType match {
                  case DRecord =>
                    w.w(s"""if (j.contains("${name}"))""").braced {
                      w.wl(s"""result.${name} = j.at("${name}").get<${marshal.fqTypename(fields(i).ty)}>();""")
                    }
                  case _ =>
                }
                case _ =>
                  w.w(s"""if (j.contains("${name}"))""").braced {
                    w.wl(s"""j.at("${name}").get_to(result.${name});""")
                  }
              }
            }
            w.wl("return result;")
          }
          // To JSON
          w.w(s"static void to_json(json & j, $recordSelf item) ").braced {
            w.w(s"j = json").braced {
              for (i <- fields.indices) {
                val name = idCpp.field(fields(i).ident)
                val comma = if (i < (fields).length - 1) "," else ""

                fields(i).ty.resolved.base match {
                  case _ => w.wl(s"""{"${name}", item.${name}}${comma}""")
                }
              }
            }
            w.wl(";")
          }
        }
        w.wl(";")
      }
    }

//    writeHppFile(cppName, origin, refs.hpp, refs.hppFwds, writeCppPrototype, writeJsonExtension)
    writeHppFile(cppName, origin, refs.hpp, refs.hppFwds, writeCppPrototype)

    // if (r.consts.nonEmpty || r.derivingTypes.contains(DerivingType.Eq) || r.derivingTypes.contains(DerivingType.Ord)) {
    writeCppFile(cppName, origin, refs.cpp, w => {
      generateCppConstants(w, r.consts, actualSelf)

      if (r.derivingTypes.contains(DerivingType.Eq)) {
        w.wl
        w.w(s"bool operator==(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
          if (fields.nonEmpty) {
            writeAlignedCall(w, "return ", fields, " &&", "", f => s"lhs.${idCpp.field(f.ident)} == rhs.${idCpp.field(f.ident)}")
            w.wl(";")
          } else {
            w.wl("return true;")
          }
        }
        w.wl
        w.w(s"bool operator!=(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
          w.wl("return !(lhs == rhs);")
        }
      }

      if (r.derivingTypes.contains(DerivingType.Ord)) {
        w.wl
        w.w(s"bool operator<(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
          for (f <- fields) {
            w.w(s"if (lhs.${idCpp.field(f.ident)} < rhs.${idCpp.field(f.ident)})").braced {
              w.wl("return true;")
            }
            w.w(s"if (rhs.${idCpp.field(f.ident)} < lhs.${idCpp.field(f.ident)})").braced {
              w.wl("return false;")
            }
          }
          w.wl("return false;")
        }

        if (spec.cppDefaultConstructor) {
          w.wl
          w.w(s"bool operator>(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
            w.wl("return rhs < lhs;")
          }
        }
      }

      if (r.derivingTypes.contains(DerivingType.Eq) && r.derivingTypes.contains(DerivingType.Ord)) {
        w.wl
        w.w(s"bool operator<=(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
          w.wl("return !(rhs < lhs);")
        }
        w.wl
        w.w(s"bool operator>=(const $actualSelf& lhs, const $actualSelf& rhs)").braced {
          w.wl("return !(lhs < rhs);")
        }
      }

//      w.w(s"std::string $actualSelf::description() const").braced {
//        w.wl("return ((nlohmann::json)(*this)).dump(2);")
//      }
    })
  }

  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface, deprecated: scala.Option[Deprecated]) {
    val refs = new CppRefs(ident.name)
    i.methods.foreach(m => {
      m.params.foreach(p => refs.find(p.ty, forwardDeclareOnly = true))
      m.ret.foreach(x => refs.find(x, forwardDeclareOnly = true))
    })
    i.consts.foreach(c => {
      refs.find(c.ty, forwardDeclareOnly = true)
    })

    val self = marshal.typename(ident, i)
    val methodNamesInScope = i.methods.map(m => idCpp.method(m.ident))

    writeHppFile(ident, origin, refs.hpp, refs.hppFwds, w => {
      writeDoc(w, doc)
      writeCppTypeParams(w, typeParams)
      marshal.deprecatedAnnotation(deprecated).foreach(w.wl)
      w.w(s"class $self").bracedSemi {
        w.wlOutdent("public:")
        // Destructor
        w.wl(s"virtual ~$self() {}")
        // Constants
        generateHppConstants(w, i.consts)
        // Methods
        for (m <- i.methods) {
          w.wl
          writeMethodDoc(w, m, idCpp.local)
          marshal.deprecatedAnnotation(m.deprecated).foreach(w.wl)

          val ret = marshal.returnType(m.ret, methodNamesInScope)
          val params = m.params.map(p => marshal.paramType(p.ty, methodNamesInScope) + " " + idCpp.local(p.ident))
          if (m.static) {
            w.wl(s"static $ret ${idCpp.method(m.ident)}${params.mkString("(", ", ", ")")};")
          } else {
            val constFlag = if (m.const) " const" else ""
            w.wl(s"virtual $ret ${idCpp.method(m.ident)}${params.mkString("(", ", ", ")")}$constFlag = 0;")
          }
        }
      }
    })

    // Cpp only generated in need of Constants
    if (i.consts.nonEmpty) {
      writeCppFile(ident, origin, refs.cpp, w => {
        generateCppConstants(w, i.consts, self)
      })
    }

  }

  def writeCppTypeParams(w: IndentWriter, params: Seq[TypeParam]) {
    if (params.isEmpty) return
    w.wl("template " + params.map(p => "typename " + idCpp.typeParam(p.ident)).mkString("<", ", ", ">"))
  }

}
