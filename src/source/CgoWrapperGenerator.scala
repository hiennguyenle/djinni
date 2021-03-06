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
    val list_name = s"${marshal.cgoWrapperTypeName(tm)}"
    val cgo_type_name = marshal.cgoWrapperTypeName(tm.args.head)
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

    hpp.add(s"#include " + q(list_name + ".h"))
    val init_func = s"${list_name}__init"
    val className = "Djinni" + idCpp.ty(name)

    // Write extern C API
    writeCHeader(list_name, origin, "", h, create = true, ".h", w => {
      writeAsExternC(w, w => {
        w.wl(s"#define $init_func {0, NULL}")
        w.wl
        w.w(s"typedef struct ${list_name}_").bracedEnd(s" $list_name;") {
          w.wl(s"size_t length;")
          w.wl(s"$cgo_type_name * data;")
        }
        w.wl
        w.wl(s"void ${list_name}__delete($list_name *);")
      })
    })

    // Write Hpp
    writeCHeader(list_name, origin, "", h ++ hpp, create = true, ".hpp", w => {
      w.wl
      w.wl(s"struct $className").bracedSemi {
        w.wl(s"static std::unique_ptr<$list_name> from_cpp(const $cpp_type_name & data);")
        w.wl(s"static $cpp_type_name to_cpp($list_name * data);")
      }
    })

    // Write Cpp
    val cpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    cpp.add(s"#include " + q(list_name + ".hpp"))
    writeCFile(list_name, origin, "", cpp, create = true, w => {
      w.wl

      w.wl(s"void ${list_name}__delete($list_name *ptr)").bracedSemi {
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
      w.wl(s"std::unique_ptr<$list_name> $className::from_cpp(const $cpp_type_name & data)").bracedSemi {
        w.wl(s"$list_name * cgo = new $list_name({0, NULL});")
        w.wl(s"if (!data.empty())").braced {
          w.wl("cgo->length = data.size();")

          val size = s"cgo->length"
          val data = s"cgo->data"
          tm.args.head.base match {
            case p: MPrimitive =>
              w.wl(s"$data = new ${p.cName}[$size];")
              w.wl(s"for (int i = 0; i < $size; i++)").bracedSemi {
                w.wl(s"$data[i] = std::move(data[i]);")
              }
            case MString =>
              w.wl(s"$data = new $cgo_type_name[$size];")
              val className = "DjinniString"
              w.wl(s"for (int i = 0; i < $size; i++)").bracedSemi {
                w.wl(s"$data[i] = $className::from_cpp(std::move(data[i]));")
              }
            case MBinary =>
              w.wl(s"$data = new $cgo_type_name[$size];")
              val className = "DjinniBinary"
              w.wl(s"for (int i = 0; i < $size; i++)").bracedSemi {
                w.wl(s"$data[i] = $className::from_cpp(std::move(data[i]));")
              }
            case d: MDef => d.defType match {
              case DInterface => throw new NotImplementedError()
              case DRecord =>
                w.wl(s"$data = new $cgo_type_name[$size];")
                val className = "DjinniCgo" + idCpp.typeParam(d.name)
                w.wl(s"for (int i = 0; i < $size; i++)").bracedSemi {
                  w.wl(s"$data[i] = *$className::from_cpp(data[i]);")
                }
              case DEnum => throw new NotImplementedError()
            }
            case _ => throw new NotImplementedError()
          }
        }
        w.wl(s"return std::unique_ptr<$list_name>(cgo);")
      }
      w.wl
      w.wl(s"$cpp_type_name $className::to_cpp( $list_name * cgo)").bracedSemi {

        val size = s"cgo->length"
        val data = s"cgo->data"

        tm.args.head.base match {
          case _: MPrimitive => w.wl(s"return $cpp_type_name($data, $data + $size);")
          case MString =>
            val className = "DjinniString"
            w.wl(s"$cpp_type_name cpp = $cpp_type_name($size);")
            w.wl(s"for (int i = 0; i < cpp.size(); i++)").braced {
              w.wl(s"cpp[i] = $className::to_cpp($data[i]);")
            }
            w.wl(s"return cpp;")
          case MBinary =>
            val className = "DjinniBinary"
            w.wl(s"$cpp_type_name cpp = $cpp_type_name($size);")
            w.wl(s"for (int i = 0; i < cpp.size(); i++)").braced {
              w.wl(s"cpp[i] = $className::to_cpp($data[i]);")
            }
            w.wl(s"return cpp;")
          case d: MDef => d.defType match {
            case DInterface => throw new NotImplementedError()
            case DRecord =>
              val className = "DjinniCgo" + idCpp.typeParam(d.name)
              w.wl(s"$cpp_type_name cpp = $cpp_type_name($size);")
              w.wl(s"for (int i = 0; i < cpp.size(); i++)").braced {
                w.wl(s"cpp[i] = $className::to_cpp($data[i]);")
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

      val name = marshal.cgoWrapperTypeName(tm)

      if (justCollect) {
        if (tm.base == MList || tm.base == MSet || tm.base == MMap) {
          h.add("#include " + q(name + ".h"))
          hpp.add("#include " + q(name + ".hpp"))
        }
      } else {
        if (!Generator.writtenFiles.contains((name + ".h").toLowerCase())) {
          Generator.writtenFiles.put((name + ".h").toLowerCase(), name)
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
    val pointerSelf = s"$self *"
    val has_init_func = i.methods.find(m => {
      marshal.cReturnType(m.ret) == s"$pointerSelf"
    })

    writeCHeader(fileName, origin, "", refs.h, create = true, ".h", w => {
      writeAsExternC(w, w => {
        w.wl(s"struct $self;")
        w.wl
        for (m <- i.methods) {
          var cFuncReturnType = marshal.cReturnType(m.ret)

          if (cFuncReturnType == s"$pointerSelf") {
            cFuncReturnType = "struct " + cFuncReturnType
          }

          val params = getDefArgs(m, self = "struct " + self + " * cgo_this")
          w.wl(s"$cFuncReturnType $prefix${idCpp.method(m.ident.name)}$params;")
          w.wl
        }

        if (has_init_func.isDefined) {
          w.wl(s"void ${prefix}delete(struct $pointerSelf ptr);")
        }
      })
    })

    writeCFile(fileName, origin, "", refs.hpp, create = true, w => {
      if (has_init_func.isDefined) {
        w.wl
        w.wl(s"static std::shared_ptr<${cppMarshal.fqTypename(ident.name, i)}> _ptr_holder;")
      }
      for (m <- i.methods) {
        w.wl
        val cFuncReturnType = marshal.cReturnType(m.ret)
        val params = getDefArgs(m, self = pointerSelf + " cgo_this")
        val name = idCpp.method(m.ident.name)
        w.wl(s"$cFuncReturnType $prefix$name$params").bracedSemi {
          if (m.static) {
            if (cFuncReturnType == s"$self *") {
              val cppFunc = s"${cppMarshal.fqTypename(ident.name, i)}::$name"
              val params = m.params.map(p => marshal.toCpp(p.ty, idCpp.field(p.ident)))
              w.wl(s"_ptr_holder = $cppFunc${params.mkString("(", ", ", ")")};")
              w.wl(s"return reinterpret_cast<$cFuncReturnType>(_ptr_holder.get());")
            } else {
              val params = m.params.map(p => marshal.toCpp(p.ty, idCpp.field(p.ident)))
              val cppFunc = s"${cppMarshal.fqTypename(ident.name, i)}::$name"
              val ret = s"$cppFunc${params.mkString("(", ", ", ")")}"
              if (m.ret.isDefined) {
                w.wl(s"return ${marshal.fromCpp(m.ret.get, ret)};")
              } else {
                w.wl(s"$ret;")
              }
            }
          } else {
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

    val cpp_type_name = cppMarshal.fqTypename(ident, r)
    val cgo_type_name = marshal.cgo + ident.name

    val className = "Djinni" + idCpp.ty(cgo_type_name)

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
    refs.hpp.add("#include <memory>")
    refs.cpp.add("#include " + marshal.cppHeader(ident))

    writeCHeader(cgo_type_name, origin, "", refs.h, create = true, ".h", w => {
      writeAsExternC(w, w => {
        w.w(s"typedef struct ${cgo_type_name}_").bracedEnd(s" ${cgo_type_name};") {
          for (f <- fields) {
            writeDoc(w, f.doc)
            w.wl(marshal.cReturnType(Some(f.ty)) + " " + idCpp.field(f.ident) + ";")
          }
        }

        w.wl
        w.wl(s"void ${cgo_type_name}__delete($cgo_type_name *);")
        w.wl(s"#define ${cgo_type_name}__init {}")
      })
    })

    writeCHeader(cgo_type_name, origin, "", refs.hpp, create = true, ".hpp", w => {
      w.wl(s"struct $className").bracedSemi {
        w.wl(s"static std::unique_ptr<$cgo_type_name> from_cpp(const $cpp_type_name & cpp);")
        w.wl(s"static std::unique_ptr<$cgo_type_name> from_cpp(const std::optional<$cpp_type_name> & cpp);")
        w.wl(s"static $cpp_type_name to_cpp(const $cgo_type_name & cgo);")
        w.wl(s"static std::optional<$cpp_type_name> to_cpp($cgo_type_name * cgo);")
      }
    })

    // Write Cpp
    writeCFile(cgo_type_name, origin, "", refs.cpp, create = true, w => {
      w.wl
      w.wl(s"void ${cgo_type_name}__delete($cgo_type_name * ptr)").bracedSemi {
        for (f <- fields) {
          val field_name = s"ptr->${idCpp.field(f.ident)}"
          val free_memory = marshal.free_memory(f.ty.resolved, field_name)
          if (free_memory != null) {
            w.wl(s"${free_memory.get};")
          }
        }
      }

      w.wl
      w.wl(s"std::unique_ptr<$cgo_type_name> $className::from_cpp(const $cpp_type_name & cpp)").bracedSemi {
        val skipFirst = SkipFirst()
        w.wl(s"auto ax = new $cgo_type_name ").bracedEnd(";") {
          for (i <- fields.indices) {
            val field = fields(i)
            val name = idCpp.field(field.ident)
            skipFirst {
              w.wl(",")
            }
            w.wl(marshal.fromCpp(field.ty, s"cpp.$name"))
          }
        }
        w.wl(s"return std::unique_ptr<$cgo_type_name>(ax);")
      }
      w.wl
      w.wl(s"$cpp_type_name $className::to_cpp(const $cgo_type_name & cgo)").bracedSemi {
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
      w.wl(s"std::unique_ptr<$cgo_type_name> $className::from_cpp(const std::optional<$cpp_type_name> & cpp)").bracedSemi {
        w.wl(s"if (cpp.has_value())").braced {
          w.wl(s"return $className::from_cpp(std::move(*cpp));")
        }
        w.wl("return nullptr;")
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