/**
 * Copyright 2015 Dropbox, Inc.
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

import djinni.ast._
import djinni.generatorTools.{q, _}
import djinni.meta._
import djinni.writer.IndentWriter

import scala.collection.mutable

class CgoWrapperGenerator(spec: Spec) extends Generator(spec) {
  val marshal = new CgoWrapperMarshal(spec)
  val cppMarshal = new CppMarshal(spec)

  def writeAsExternC(w: IndentWriter, f: IndentWriter => Unit): Unit = {
    w.wl("#ifdef __cplusplus")
    w.wl("extern \"C\" {")
    w.wl("#endif")
    w.wl
    f(w)
    w.wl
    w.wl("#ifdef __cplusplus")
    w.wl("}")
    w.wl("#endif")
  }

  def writeCFile(fileName: String, ident: String, origin: String, includes: Iterable[String], create: Boolean, f: IndentWriter => Unit) {
    if (create) createFile(spec.cgoWrapperOutFolder.get, fileName + ".cpp", (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      w.wl("#include \"cgo_wrapper_marshal.hpp\"")
      if (includes.nonEmpty) {
        w.wl
        includes.foreach(w.wl)
      }
    })

    appendToFile(spec.cgoWrapperOutFolder.get, fileName + ".cpp", (w: IndentWriter) => {
      f(w)
    })
  }

  def writeCHeader(fileName: String, origin: String, cppClass: String, includes: Iterable[String], create: Boolean, ext: String = ".h", f: IndentWriter => Unit): Unit = {
    if (create) createFile(spec.cgoWrapperOutFolder.get, fileName + ext, (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      w.wl("#pragma once")
      w.wl
      w.wl("#include \"cgo_wrapper_marshal.h\"")
      if (includes.nonEmpty) {
        w.wl
        includes.foreach(w.wl)
      }
    })

    appendToFile(spec.cgoWrapperOutFolder.get, fileName + ext, (w: IndentWriter) => {
      w.wl
      f(w)
    })
  }

  def generateList(tm: MExpr, name: String, ident: Ident, origin: String): Unit = {
    val struct_name = s"${marshal.cgoWrapperType(tm)}"
    val cgo_type_name = marshal.cgoWrapperType(tm.args.head)
    val cpp_type_name = cppMarshal.fqTypename(tm)

    // Collect h header files
    val h: mutable.TreeSet[String] = mutable.TreeSet[String]()
    for (r <- marshal.hReferences(tm.args.head.base, ident)) r match {
      case ImportRef(arg) =>
        h.add("#include " + arg)
      case _ =>
    }

    // Collect hpp header files
    val hpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    for (r <- marshal.hppReferences(tm.args.head.base, ident)) r match {
      case ImportRef(arg) =>
        hpp.add("#include " + arg)
      case _ =>
    }

    for (r <- marshal.hppReferences(tm.base, ident)) r match {
      case ImportRef(arg) =>
        hpp.add("#include " + arg)
      case _ =>
    }

    hpp.add(s"#include " + q(struct_name + ".h"))
    val init_func = s"${struct_name}__init"
    val className = "Djinni" + idCpp.ty(name)

    // Write extern C API
    writeCHeader(struct_name, origin, "", h, create = true, ".h", w => {
      writeAsExternC(w, w => {
        w.wl(s"#define $init_func {0, NULL}")
        w.wl
        w.w(s"typedef struct ${struct_name}_").bracedEnd(s" $struct_name;") {
          w.wl(s"size_t length;")
          w.wl(s"$cgo_type_name * data;")
        }
        w.wl
        w.wl(s"void ${struct_name}__delete($struct_name *);")
      })
    })

    // Write Hpp
    writeCHeader(struct_name, origin, "", h ++ hpp, create = true, ".hpp", w => {
      w.wl
      w.wl(s"struct $className").bracedSemi {
        w.wl(s"static $struct_name from_cpp(const $cpp_type_name & data);")
        w.wl(s"static $cpp_type_name to_cpp(const $struct_name & data);")
      }
    })

    // Write Cpp
    val cpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    cpp.add(s"#include " + q(struct_name + ".hpp"))
    writeCFile(struct_name, origin, "", cpp, create = true, w => {
      w.wl

      w.wl(s"void ${struct_name}__delete($struct_name *ptr)").bracedSemi {
        w.wl(s"if (ptr == nullptr)").braced {
          w.wl("return;")
        }
        w.wl(s"if (ptr->data == nullptr)").braced {
          w.wl("return;")
        }
        w.wl(s"delete ptr->data;")
        w.wl(s"ptr->data = NULL;")
      }
      w.wl
      w.wl(s"$struct_name $className::from_cpp(const $cpp_type_name & data)").bracedSemi {
        w.wl(s"$struct_name cgo = $init_func;")
        w.wl(s"if (data.empty())").braced {
          w.wl("return cgo;")
        }
        w.wl
        w.wl(s"cgo.length = data.size();")
        tm.args.head.base match {
          case p: MPrimitive =>
            w.wl(s"cgo.data = new ${p.cName}[cgo.length];")
            w.wl(s"for (int i = 0; i < cgo.length; i++)").bracedSemi {
              w.wl("cgo.data[i] = std::move(data[i]);")
            }
          case MString =>
            w.wl(s"cgo.data = new $cgo_type_name[cgo.length];")
            val className = "DjinniString"
            w.wl(s"for (int i = 0; i < cgo.length; i++)").bracedSemi {
              w.wl(s"cgo.data[i] = $className::from_cpp(data[i]);")
            }
          case d: MDef => d.defType match {
            case DInterface => throw new NotImplementedError()
            case DRecord =>
              w.wl(s"cgo.data = new $cgo_type_name[cgo.length];")
              val className = "DjinniCgo" + idCpp.typeParam(d.name)
              w.wl(s"for (int i = 0; i < cgo.length; i++)").bracedSemi {
                w.wl(s"cgo.data[i] = $className::from_cpp(data[i]);")
              }
            case DEnum => throw new NotImplementedError()
          }
          case _ => throw new NotImplementedError()
        }
        w.wl("return cgo;")
      }
      w.wl
      w.wl(s"$cpp_type_name $className::to_cpp(const $struct_name & cgo)").bracedSemi {
        tm.args.head.base match {
          case _: MPrimitive => w.wl(s"return $cpp_type_name(cgo.data, cgo.data + cgo.length);")
          case MString =>
            val className = "DjinniString"
            w.wl(s"$cpp_type_name cpp = $cpp_type_name(cgo.length);")
            w.wl(s"for (int i = 0; i < cpp.size(); i++)").braced {
              w.wl(s"cpp[i] = $className::to_cpp(cgo.data[i]);")
            }
            w.wl(s"return cpp;")
          case d: MDef => d.defType match {
            case DInterface => throw new NotImplementedError()
            case DRecord =>
              val className = "DjinniCgo" + idCpp.typeParam(d.name)
              w.wl(s"$cpp_type_name cpp = $cpp_type_name(cgo.length);")
              w.wl(s"for (int i = 0; i < cpp.size(); i++)").braced {
                w.wl(s"cpp[i] = $className::to_cpp(cgo.data[i]);")
              }
              w.wl(s"return cpp;")
            case DEnum => throw new NotImplementedError()
          }
          case _ => throw new NotImplementedError()
        }
      }
    })
  }

  def generateMap(tm: MExpr, name: String, ident: Ident, origin: String, h: mutable.TreeSet[String], hpp: mutable.TreeSet[String]): Unit = {

  }

  def generateSet(tm: MExpr, name: String, ident: Ident, origin: String, h: mutable.TreeSet[String], hpp: mutable.TreeSet[String]): Unit = {

  }

  class CppRefs(ident: Ident, origin: String) {
    var cpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    var hpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    var h: mutable.TreeSet[String] = mutable.TreeSet[String]()

    def find(ty: TypeRef, justCollect: Boolean) {
      find(ty.resolved, justCollect)
    }

    def find(tm: MExpr, justCollect: Boolean) {
      tm.args.foreach(t => find(t, justCollect))

      find(tm.base, justCollect)

      val name = marshal.cgoWrapperType(tm)
      val fileName = name

      if (justCollect) {
        if (tm.base == MList || tm.base == MSet || tm.base == MMap) {
          h.add("#include " + q(fileName + ".h"))
          hpp.add("#include " + q(fileName + ".hpp"))
        }
      } else {
        if (!Generator.writtenFiles.contains((fileName + ".h").toLowerCase())) {
          Generator.writtenFiles.put((fileName + ".h").toLowerCase(), fileName)
          tm.base match {
            case MList => generateList(tm, name, ident, origin)
            case MMap => generateMap(tm, name, ident, origin, h, hpp)
            case MSet => generateSet(tm, name, ident, origin, h, hpp)
            case _ =>
          }
        }
      }
    }

    def find(m: Meta, justCollect: Boolean): Unit = {
      if (justCollect) {
        for (r <- marshal.hReferences(m, ident)) r match {
          case ImportRef(arg) =>
            h.add("#include " + arg)
          case _ =>
        }

        for (r <- marshal.hppReferences(m, ident)) r match {
          case ImportRef(arg) =>
            hpp.add("#include " + arg)
          case _ =>
        }
      }
    }
  }

  def generateEnum(origin: String, ident: djinni.ast.Ident, doc: djinni.ast.Doc, e: djinni.ast.Enum, deprecated: Option[djinni.ast.Deprecated]) {
    val refs = new CppRefs(ident, origin)
    // Include cpp header
    refs.hpp.add("#include " + q(ident.name + ".hpp"))
    refs.hpp.add("#include " + marshal.cHeader(ident))
    refs.hpp.add("#include <optional>")
    refs.cpp.add("#include " + marshal.cppHeader(ident))

    val fileName = marshal.cgo + ident.name
    val className = "Djinni" + idCpp.ty(fileName)
    val skipFirst = SkipFirst()

    writeCHeader(fileName, origin, "", refs.h, create = true, ".h", w => {
      w.w(s"typedef enum").braced {
        for (o <- e.options) {
          skipFirst {
            w.wl(",")
          }
          o.value match {
            case Some(i)=> w.w(s"${fileName}_${o.ident.name} = $i")
            case None => w.w(s"${fileName}_${o.ident.name}")
          }
        }
      }
      w.wl(s"$fileName;")
    })

    val cpp_type_name = cppMarshal.fqTypename(ident, e)
    val cgo_type_name = marshal.cgo + ident.name

    writeCHeader(fileName, origin, "", refs.hpp, create = true, ".hpp", w => {
      w.wl(s"struct $className").bracedSemi {
        w.wl(s"static $cgo_type_name from_cpp(const $cpp_type_name & cpp);")
        w.wl(s"static $cpp_type_name to_cpp(const $cgo_type_name & cgo);")
        w.wl(s"static std::optional<$cgo_type_name> from_cpp(const std::optional<$cpp_type_name> & cpp);")
        w.wl(s"static std::optional<$cpp_type_name> to_cpp($cgo_type_name * cgo);")
      }
    })

    writeCFile(fileName, origin, "", refs.cpp, create = true, w => {
      w.wl
      w.wl(s"$fileName $className::from_cpp(const $cpp_type_name & cpp)").bracedSemi {
        w.wl(s"return static_cast<$fileName>(cpp);")
      }
      w.wl
      w.wl(s"$cpp_type_name $className::to_cpp(const $fileName & cgo)").bracedSemi {
        w.wl(s"return static_cast<$cpp_type_name>(cgo);")
      }
      // Optional
      w.wl
      w.wl(s"std::optional<$cgo_type_name> $className::from_cpp(const std::optional<$cpp_type_name> & cpp)").bracedSemi {
        w.wl(s"if (cpp.has_value())").braced {
          w.wl(s"return $className::from_cpp(cpp.value());")
        }
        w.wl("return std::nullopt;")
      }

      w.wl
      w.wl(s"std::optional<$cpp_type_name> $className::to_cpp($cgo_type_name * cgo)").bracedSemi {
        w.wl(s"if (cgo != NULL)").braced {
          w.wl(s"return $className::to_cpp(*cgo);")
        }
        w.wl("return std::nullopt;")
      }
    })
  }

  def generateInterface(origin: String,
                        ident: djinni.ast.Ident,
                        doc: djinni.ast.Doc,
                        typeParams: Seq[djinni.ast.TypeParam],
                        i: djinni.ast.Interface,
                        deprecated: Option[djinni.ast.Deprecated]) {

    val refs = new CppRefs(ident, origin)
    i.methods.foreach(m => {
      m.params.foreach(p => refs.find(p.ty, justCollect = true))
      m.ret.foreach(t => refs.find(t, justCollect = true))

      m.params.foreach(p => refs.find(p.ty, justCollect = false))
      m.ret.foreach(t => refs.find(t, justCollect = false))
    })

    i.consts.foreach(c => {
      refs.find(c.ty, justCollect = true)
    })

    refs.hpp.add("#include " + marshal.cHeader(ident))
    refs.hpp.add("#include " + q(ident.name + ".hpp"))

    val fileName = marshal.cgo + ident.name
    val self = fileName
    val prefix = s"${self}__"
    writeCHeader(fileName, origin, "", refs.h, create = true, ".h", w => {
      writeAsExternC(w, w => {

        w.wl(s"struct $self;")
        w.wl
        for (m <- i.methods) {
          var cFuncReturnType = marshal.cReturnType(m.ret)

          if (cFuncReturnType == s"$self *") {
            cFuncReturnType = "struct " + cFuncReturnType
          }

          val params = getDefArgs(m, self = "struct " + self + " * cgo_this")
          w.wl(s"$cFuncReturnType $prefix${idCpp.method(m.ident.name)}$params;")
          w.wl
        }

        w.wl(s"void ${prefix}delete(struct $self * ptr);")
      })
    })

    writeCFile(fileName, origin, "", refs.hpp, create = true, w => {
      w.wl
      w.wl(s"static std::shared_ptr<${cppMarshal.fqTypename(ident.name, i)}> _ptr_holder;")
      w.wl
      w.wl(s"void ${prefix}delete(struct $self * ptr)").bracedSemi {
        w.wl("_ptr_holder.reset();")
      }

      for (m <- i.methods) {
        w.wl
        val cFuncReturnType = marshal.cReturnType(m.ret)
        val params = getDefArgs(m, self = self + " * cgo_this")
        val name = idCpp.method(m.ident.name)
        w.wl(s"$cFuncReturnType $prefix$name$params").bracedSemi {
          if (m.static) {
            if (cFuncReturnType == s"$self *") {
              val cppFunc = s"${cppMarshal.fqTypename(ident.name, i)}::$name"
              val params = m.params.map(p => marshal.toCpp(p.ty, idCpp.field(p.ident)))
              w.wl(s"_ptr_holder = $cppFunc${params.mkString("(", ", ", ")")};")
              w.wl(s"return reinterpret_cast<$cFuncReturnType>(_ptr_holder.get());")
            } else {
              val cppRef = cppMarshal.fqTypename(ident.name, i)
              val params = m.params.map(p => marshal.toCpp(p.ty, idCpp.field(p.ident)))
              val cppFunc = s"${cppMarshal.fqTypename(ident.name, i)}::$name"
              val ret = s"$cppFunc${params.mkString("(", ", ", ")")}"
              if (m.ret.isDefined) {
                w.wl(s"return ${marshal.fromCpp(m.ret.get, ret)};")
              } else {
                w.wl(s"$ret;")
              }
            }
          }
          else {
            val cppRef = cppMarshal.fqTypename(ident.name, i)
            w.wl(s"$cppRef * ptr = reinterpret_cast<$cppRef*>(cgo_this);")
            val params = m.params.map(p => marshal.toCpp(p.ty, idCpp.field(p.ident)))
            val ret = s"ptr->$name${params.mkString("(", ", ", ")")}"
            if (m.ret.isDefined) {
              w.wl(s"return ${marshal.fromCpp(m.ret.get, ret)};")
            } else {
              w.wl(s"$ret;")
            }
          }
        }
        w.wl
      }
    })
  }

  def getDefArgs(m: Interface.Method, self: String): String = {
    if (m.static) {
      marshal.cArgDecl(m.params.map(p => marshal.cgoWrapperType(p.ty.resolved) + " " + idCpp.local(p.ident.name)))
    } else {
      marshal.cArgDecl(Seq[String](self) ++
        m.params.map(p => marshal.cgoWrapperType(p.ty.resolved) + " " + idCpp.local(p.ident.name)))
    }
  }

  def generateRecord(origin: String,
                     ident: djinni.ast.Ident,
                     doc: djinni.ast.Doc,
                     params: Seq[djinni.ast.TypeParam],
                     r: djinni.ast.Record,
                     deprecated: Option[djinni.ast.Deprecated],
                     idl: Seq[djinni.ast.TypeDecl]) {

    val refs = new CppRefs(ident, origin)

    val structName = marshal.cgo + ident.name
    val superRecord = getSuperRecord(idl, r)
    val superFields: Seq[Field] = superRecord match {
      case None => Seq.empty
      case Some(value) => value.fields
    }

    val fields: Seq[Field] = superFields ++ r.fields

    fields.foreach(f => refs.find(f.ty, justCollect = true))
    fields.foreach(f => refs.find(f.ty, justCollect = false))

    // Include cpp header
    refs.hpp.add("#include " + q(ident.name + ".hpp"))
    refs.hpp.add("#include " + marshal.cHeader(ident))
    refs.hpp.add("#include <optional>")
    refs.cpp.add("#include " + marshal.cppHeader(ident))

    writeCHeader(structName, origin, "", refs.h, create = true, ".h", w => {
      writeAsExternC(w, w => {
        w.w(s"typedef struct ${structName}_").bracedEnd(s" ${structName};") {
          for (f <- fields) {
            writeDoc(w, f.doc)
            w.wl(marshal.toCwrapperType(f.ty.resolved, forHeader = false) + " " + idCpp.field(f.ident) + ";")
          }
        }

        w.wl
        w.wl(s"void ${structName}__delete($structName *);")
        w.wl(s"#define ${structName}__init {}")
      })
    })

    val className = "Djinni" + idCpp.ty(structName)
    val cpp_type_name = cppMarshal.fqTypename(ident, r)
    val cgo_type_name = marshal.cgo + ident.name

    writeCHeader(structName, origin, "", refs.hpp, create = true, ".hpp", w => {
      w.wl(s"struct $className").bracedSemi {
        w.wl(s"static $cgo_type_name from_cpp(const $cpp_type_name & cpp);")
        w.wl(s"static $cpp_type_name to_cpp(const $cgo_type_name & cgo);")

        w.wl(s"static std::optional<$cgo_type_name> from_cpp(const std::optional<$cpp_type_name> & cpp);")
        w.wl(s"static std::optional<$cpp_type_name> to_cpp($cgo_type_name * cgo);")
      }
    })

    // Write Cpp
    writeCFile(structName, origin, "", refs.cpp, create = true, w => {
      w.wl
      w.wl(s"void ${structName}__delete($structName * ptr)").bracedSemi {
        for (f <- fields) {
          val field_name = s"ptr->${idCpp.field(f.ident)}"
          val free_memory = marshal.free_memory(f.ty.resolved, field_name)
          if (free_memory != null) {
            w.wl(s"${free_memory.get};")
          }
        }
      }

      w.wl
      w.wl(s"$structName $className::from_cpp(const $cpp_type_name & cpp)").bracedSemi {
        val skipFirst = SkipFirst()
        w.wl("return ").bracedSemi {
          for (i <- fields.indices) {
            val field = fields(i)
            val name = idCpp.field(field.ident)
            skipFirst {
              w.wl(",")
            }
            w.w(marshal.fromCpp(field.ty, s"cpp.$name"))
          }
        }
      }
      w.wl
      w.wl(s"$cpp_type_name $className::to_cpp(const $structName & cgo)").bracedSemi {
        val skipFirst = SkipFirst()
        w.wl("return ").bracedSemi {
          for (i <- fields.indices) {
            val field = fields(i)
            val name = idCpp.field(field.ident)
            skipFirst {
              w.wl(",")
            }
            w.w(marshal.toCpp(field.ty, s"cgo.$name"))
          }
        }
      }

      // Optional
      w.wl
      w.wl(s"std::optional<$cgo_type_name> $className::from_cpp(const std::optional<$cpp_type_name> & cpp)").bracedSemi {
        w.wl(s"if (cpp.has_value())").braced {
          w.wl(s"return $className::from_cpp(cpp.value());")
        }
        w.wl("return std::nullopt;")
      }

      w.wl
      w.wl(s"std::optional<$cpp_type_name> $className::to_cpp($cgo_type_name * cgo)").bracedSemi {
        w.wl(s"if (cgo != NULL)").braced {
          w.wl(s"return $className::to_cpp(cgo);")
        }
        w.wl("return std::nullopt;")
      }
    })

  }
}