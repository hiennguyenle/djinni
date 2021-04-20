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

import java.io._
import djinni.ast._
import djinni.generatorTools.{JavaAccessModifier, _}
import djinni.writer.IndentWriter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.util.matching.Regex

package object generatorTools {
  type IdentConverter = String => String

  def preComma(s: String) = {
    if (s.isEmpty) s else ", " + s
  }

  def p(s: String) = "(" + s + ")"

  def t(s: String) = "<" + s + ">"

  def firstUpper(token: String) = if (token.isEmpty()) token else token.charAt(0).toUpper + token.substring(1)

  def generate(idl: Seq[TypeDecl], spec: Spec): Option[String] = {
    try {
      if (spec.cppOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("C++", spec.cppOutFolder.get)
          createFolder("C++ header", spec.cppHeaderOutFolder.get)
        }
        new CppGenerator(spec).generate(idl)
      }
      if (spec.javaOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("Java", spec.javaOutFolder.get)
        }
        new JavaGenerator(spec).generate(idl)
      }
      if (spec.jniOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("JNI C++", spec.jniOutFolder.get)
          createFolder("JNI C++ header", spec.jniHeaderOutFolder.get)
        }
        new JNIGenerator(spec).generate(idl)
      }

      if (spec.objcppOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("Objective-C++", spec.objcppOutFolder.get)
        }
        new ObjcppGenerator(spec).generate(idl)
      }

      if (spec.objcOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("Objective-C", spec.objcOutFolder.get)
        }
        new ObjcGenerator(spec).generate(idl)
      }

      if (spec.swiftOutFolder.isDefined) {
        DEBUG(spec.swiftOutFolder.get.toString)
        if (!spec.skipGeneration) {
          createFolder("Swift", spec.swiftOutFolder.get)
        }
        new SwiftGenerator(spec).generate(idl)
      }

      if (spec.objcSwiftBridgingHeaderWriter.isDefined) {
        SwiftSupportingFilesGenerator.writeAutogenerationWarning(spec.objcSwiftBridgingHeaderName.get, spec.objcSwiftBridgingHeaderWriter.get)
        SwiftSupportingFilesGenerator.writeBridgingVars(spec.objcSwiftBridgingHeaderName.get, spec.objcSwiftBridgingHeaderWriter.get)
        val generator = new SwiftSupportingFilesGenerator(spec)
        generator.generate(idl)
      }

      if (spec.yamlOutFolder.isDefined) {
        if (!spec.skipGeneration) {
          createFolder("YAML", spec.yamlOutFolder.get)
        }
        new YamlGenerator(spec).generate(idl)
      }

      if (spec.pyOutFolder.isDefined) {
        DEBUG(spec.pyOutFolder.get.toString)
        if (!spec.skipGeneration) {
          createFolder("Python", spec.pyOutFolder.get)
        }
        new PythonGenerator(spec).generate(idl)
      }

      if (spec.cWrapperOutFolder.isDefined) {
        DEBUG(spec.cWrapperOutFolder.get.toString)
        if (!spec.skipGeneration) {
          createFolder("C", spec.cWrapperOutFolder.get)
        }
         new CWrapperGenerator(spec).generate(idl)
      }

      if (spec.cgoWrapperOutFolder.isDefined) {
        DEBUG(spec.cgoWrapperOutFolder.get.toString)
        if (!spec.skipGeneration) {
          createFolder("Cgo wrapper", spec.cgoWrapperOutFolder.get)
        }
        new CgoWrapperGenerator(spec).generate(idl)
      }

      if (spec.pycffiOutFolder.isDefined) {
        DEBUG(spec.pycffiOutFolder.get.toString)
        if (!spec.skipGeneration) {
          createFolder("Cffi", spec.pycffiOutFolder.get)
        }
        new CffiGenerator(spec).generate(idl)
      }

      None
    }
    catch {
      case GenerateException(message) => Some(message)
    }
  }

  def createFolder(name: String, folder: File) {
    folder.mkdirs()
    if (folder.exists) {
      if (!folder.isDirectory) {
        throw new GenerateException(s"Unable to create $name folder at ${q(folder.getPath)}, there's something in the way.")
      }
    } else {
      throw new GenerateException(s"Unable to create $name folder at ${q(folder.getPath)}.")
    }
  }

  def q(s: String) = '"' + s + '"'

  def DEBUG(s: String) = System.out.println(s)

  sealed abstract class SymbolReference

  case class Spec(
                   javaOutFolder: Option[File],
                   javaPackage: Option[String],
                   javaClassAccessModifier: JavaAccessModifier.Value,
                   javaIdentStyle: JavaIdentStyle,
                   javaCppException: Option[String],
                   javaAnnotation: Option[String],
                   javaGenerateInterfaces: Boolean,
                   javaNullableAnnotation: Option[String],
                   javaNonnullAnnotation: Option[String],
                   javaImplementAndroidOsParcelable: Boolean,
                   javaUseFinalForRecord: Boolean,
                   cppOutFolder: Option[File],
                   cppHeaderOutFolder: Option[File],
                   cppIncludePrefix: String,
                   cppExtendedRecordIncludePrefix: String,
                   cppNamespace: String,
                   cppIdentStyle: CppIdentStyle,
                   cppFileIdentStyle: IdentConverter,
                   cppOptionalTemplate: String,
                   cppOptionalHeader: String,
                   cppEnumHashWorkaround: Boolean,
                   cppNnHeader: Option[String],
                   cppNnType: Option[String],
                   cppNnCheckExpression: Option[String],
                   cppUseWideStrings: Boolean,
                   cppDefaultConstructor: Boolean,
                   jniOutFolder: Option[File],
                   jniHeaderOutFolder: Option[File],
                   jniIncludePrefix: String,
                   jniIncludeCppPrefix: String,
                   jniNamespace: String,
                   jniClassIdentStyle: IdentConverter,
                   jniFileIdentStyle: IdentConverter,
                   jniBaseLibIncludePrefix: String,
                   cppExt: String,
                   cppHeaderExt: String,
                   objcOutFolder: Option[File],
                   objcppOutFolder: Option[File],
                   objcIdentStyle: ObjcIdentStyle,
                   objcFileIdentStyle: IdentConverter,
                   objcppExt: String,
                   objcHeaderExt: String,
                   objcIncludePrefix: String,
                   objcExtendedRecordIncludePrefix: String,
                   objcppIncludePrefix: String,
                   objcppIncludeCppPrefix: String,
                   objcppIncludeObjcPrefix: String,
                   objcppNamespace: String,
                   objcBaseLibIncludePrefix: String,
                   objcSwiftBridgingHeaderWriter: Option[Writer],
                   objcSwiftBridgingHeaderName: Option[String],
                   objcClosedEnums: Boolean,
                   objcSupportFramework: Boolean,
                   objcSupportSwiftName: Boolean,
                   objcTypePrefix: String,
                   outFileListWriter: Option[Writer],
                   skipGeneration: Boolean,
                   yamlOutFolder: Option[File],
                   yamlOutFile: Option[String],
                   yamlPrefix: String,
                   pyOutFolder: Option[File],
                   pyPackageName: String,
                   pyIdentStyle: PythonIdentStyle,
                   pycffiOutFolder: Option[File],
                   pycffiPackageName: String,
                   pycffiDynamicLibList: String,
                   idlFileName: String,
                   cWrapperOutFolder: Option[File],
                   pyImportPrefix: String,
                   swiftIdentStyle: SwiftIdentStyle,
                   swiftOutFolder: Option[File],
                   swiftGeneratedHeader: Option[String],
                   cgoWrapperOutFolder: Option[File],
                   cppJsonExtension: Boolean)

  case class CppIdentStyle(ty: IdentConverter, enumType: IdentConverter, typeParam: IdentConverter,
                           method: IdentConverter, field: IdentConverter, local: IdentConverter,
                           enum: IdentConverter, const: IdentConverter)

  case class JavaIdentStyle(ty: IdentConverter, typeParam: IdentConverter,
                            method: IdentConverter, field: IdentConverter, local: IdentConverter,
                            enum: IdentConverter, const: IdentConverter)

  implicit val javaAccessModifierReads: scopt.Read[JavaAccessModifier.Value] = scopt.Read.reads(JavaAccessModifier withName)

  case class ObjcIdentStyle(ty: IdentConverter, typeParam: IdentConverter,
                            method: IdentConverter, field: IdentConverter, local: IdentConverter,
                            enum: IdentConverter, const: IdentConverter)

  case class PythonIdentStyle(ty: IdentConverter, className: IdentConverter, typeParam: IdentConverter,
                              method: IdentConverter, field: IdentConverter, local: IdentConverter,
                              enum: IdentConverter, const: IdentConverter)

  case class SwiftIdentStyle(ty: IdentConverter, typeParam: IdentConverter,
                             method: IdentConverter, field: IdentConverter, local: IdentConverter,
                             enum: IdentConverter, const: IdentConverter)

  final case class SkipFirst() {
    private var first = true

    def apply(f: => Unit) {
      if (first) {
        first = false
      }
      else {
        f
      }
    }
  }

  case class GenerateException(message: String) extends java.lang.Exception(message)

  case class ImportRef(arg: String) extends SymbolReference // Already contains <> or "" in C contexts

  case class DeclRef(decl: String, namespace: Option[String]) extends SymbolReference

  object IdentStyle {
    val camelUpper: String => String = (s: String) => s.split('_').map(firstUpper).mkString
    val camelLower: String => String = (s: String) => {
      val parts = s.split('_')
      parts.head + parts.tail.map(firstUpper).mkString
    }
    val underLower: String => String = (s: String) => s
    val underUpper: String => String = (s: String) => s.split('_').map(firstUpper).mkString("_")
    val underCaps: String => String = (s: String) => s.toUpperCase
    val prefix: (String, IdentConverter) => String => String = (prefix: String, suffix: IdentConverter) => (s: String) => prefix + suffix(s)

    val javaDefault: JavaIdentStyle = JavaIdentStyle(camelUpper, camelUpper, camelLower, camelLower, camelLower, underCaps, underCaps)
    val cppDefault: CppIdentStyle = CppIdentStyle(camelUpper, camelUpper, camelUpper, underLower, underLower, underLower, underCaps, underCaps)
    val objcDefault: ObjcIdentStyle = ObjcIdentStyle(camelUpper, camelUpper, camelLower, camelLower, camelLower, camelUpper, camelUpper)
    val pythonDefault: PythonIdentStyle = PythonIdentStyle(underLower, camelUpper, underLower, underLower, underLower, underLower, underUpper, underCaps)

    val swiftDefault: SwiftIdentStyle = SwiftIdentStyle(
      ty = camelUpper,
      typeParam = camelUpper,
      method = camelLower,
      field = camelLower,
      local = camelLower,
      `enum` = camelLower,
      const = camelLower)

    val styles = Map(
      "FooBar" -> camelUpper,
      "fooBar" -> camelLower,
      "foo_bar" -> underLower,
      "Foo_Bar" -> underUpper,
      "FOO_BAR" -> underCaps)

    def infer(input: String): Option[IdentConverter] = {
      styles.foreach(e => {
        val (str, func) = e
        if (input endsWith str) {
          val diff = input.length - str.length
          return Some(if (diff > 0) {
            val before = input.substring(0, diff)
            prefix(before, func)
          } else {
            func
          })
        }
      })
      None
    }
  }

  object JavaAccessModifier extends Enumeration {
    val Public: JavaAccessModifier.Value = Value("public")
    val Package: JavaAccessModifier.Value = Value("package")

    def getCodeGenerationString(javaAccessModifier: JavaAccessModifier.Value): String = {
      javaAccessModifier match {
        case Public => "public "
        case Package => "/*package*/ "
      }
    }
  }

}

object Generator {
  val writtenFiles: mutable.Map[String, String] = mutable.HashMap[String, String]()
}

abstract class Generator(spec: Spec) {

  val idCpp: CppIdentStyle = spec.cppIdentStyle
  val idJava: JavaIdentStyle = spec.javaIdentStyle
  val idObjc: ObjcIdentStyle = spec.objcIdentStyle
  val idPython: PythonIdentStyle = spec.pyIdentStyle
  val idSwift: SwiftIdentStyle = spec.swiftIdentStyle

  implicit def identToString(ident: Ident): String = ident.name

  def wrapAnonymousNamespace(w: IndentWriter, f: IndentWriter => Unit) {
    w.wl("namespace { // anonymous namespace")
    w.wl
    f(w)
    w.wl
    w.wl("} // end anonymous namespace")
  }

  def writeHppFileGeneric(folder: File, namespace: String, fileIdentStyle: IdentConverter)(name: String, origin: String, includes: Iterable[String], fwds: Iterable[String], f: IndentWriter => Unit, f2: IndentWriter => Unit) {
    createFile(folder, fileIdentStyle(name) + "." + spec.cppHeaderExt, (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      w.wl("#pragma once")
      if (includes.nonEmpty) {
        w.wl
        includes.foreach(w.wl)
      }
      w.wl
      wrapNamespace(w, namespace,
        (w: IndentWriter) => {
          if (fwds.nonEmpty) {
            fwds.foreach(w.wl)
            w.wl
          }
          f(w)
        }
      )
      f2(w)
    })
  }

  def writeCppFileGeneric(folder: File, namespace: String, fileIdentStyle: IdentConverter, includePrefix: String)(name: String, origin: String, includes: Iterable[String], f: IndentWriter => Unit) {
    createFile(folder, fileIdentStyle(name) + "." + spec.cppExt, (w: IndentWriter) => {
      w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
      w.wl("// This file generated by Djinni from " + origin)
      w.wl
      val myHeader = q(includePrefix + fileIdentStyle(name) + "." + spec.cppHeaderExt)
      w.wl(s"#include $myHeader  // my header")
      val myHeaderInclude = s"#include $myHeader"
      for (include <- includes if include != myHeaderInclude)
        w.wl(include)
      w.wl
      wrapNamespace(w, namespace, f)
    })
  }

  protected def createFile(folder: File, fileName: String, f: IndentWriter => Unit): Unit = createFile(folder, fileName, out => new IndentWriter(out), f)

  protected def createFile(folder: File, fileName: String, makeWriter: OutputStreamWriter => IndentWriter, f: IndentWriter => Unit): Unit = {
    if (spec.outFileListWriter.isDefined) {
      spec.outFileListWriter.get.write(new File(folder, fileName).getPath + "\n")
    }
    if (spec.skipGeneration) {
      return
    }

    val file = new File(folder, fileName)
    val cp = file.getCanonicalPath
    Generator.writtenFiles.put(cp.toLowerCase, cp) match {
      case Some(existing) =>
        if (existing == cp) {
          throw GenerateException("Refusing to write \"" + file.getPath + "\"; we already wrote a file to that path.")
        } else {
          throw GenerateException("Refusing to write \"" + file.getPath + "\"; we already wrote a file to a path that is the same when lower-cased: \"" + existing + "\".")
        }
      case _ =>
    }

    val fout = new FileOutputStream(file)
    try {
      val out = new OutputStreamWriter(fout, "UTF-8")
      f(makeWriter(out))
      out.flush()
    }
    finally {
      fout.close()
    }
  }

  def wrapNamespace(w: IndentWriter, ns: String, f: IndentWriter => Unit) {
    ns match {
      case "" => f(w)
      case s =>
        val parts = s.split("::")
        w.wl(parts.map("namespace " + _ + " {").mkString(" ")).wl
        f(w)
        w.wl
        w.wl(parts.map(p => "}").mkString(" ") + s"  // namespace $s")
    }
  }

  def generate(idl: Seq[TypeDecl]) {
    for (td <- idl.collect { case itd: InternTypeDecl => itd }) td.body match {
      case e: Enum => {
        assert(td.params.isEmpty)
        generateEnum(td.origin, td.ident, td.doc, e, td.deprecated)
      }
      case r: Record => {
        generateRecord(td.origin, td.ident, td.doc, td.params, r, td.deprecated, idl)
      }
      case i: Interface => {
        generateInterface(td.origin, td.ident, td.doc, td.params, i, td.deprecated)
      }
    }
  }

  def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum, deprecated: scala.Option[Deprecated])

  def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record, deprecated: scala.Option[Deprecated], idl: Seq[TypeDecl])

  def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface, deprecated: scala.Option[Deprecated])

  def collectSuperFields(idl: Seq[TypeDecl], _r: Record): Seq[Field] = {
    @tailrec
    def superFieldsAccumulator(r: Record, fields: Seq[Field]): Seq[Field] = {
      r.baseRecord match {
        case None => r.fields ++ fields
        case Some(_) =>
          val baseRecord = getSuperRecord(idl, r).get
          superFieldsAccumulator(baseRecord.record, r.fields)
      }
    }

    superFieldsAccumulator(_r, Seq.empty)
  }

  def getSuperRecord(idl: Seq[TypeDecl], r: Record): Option[SuperRecord] = {
    r.baseRecord match {
      case None => None
      case Some(value) => {
        idl.find(td => td.ident.name == value) match {
          case Some(superDec) => superDec.body match {
            case superRecord: Record => {
              val superFields = collectSuperFields(idl, superRecord)
              Some(SuperRecord(superDec.ident, superRecord, superFields))
            }
            case _ => throw new AssertionError("Unreachable. The parser throws an exception when extending a non-interface type.")
          }
          case _ => throw new AssertionError("Unreachable. The parser throws an exception when extending an interface that doesn't exist.")
        }
      }
    }
  }

  def withCppNs(t: String) = withNs(Some(spec.cppNamespace), t)

  def withNs(namespace: Option[String], t: String) = namespace match {
    case None => t
    case Some("") => "::" + t
    case Some(s) => "::" + s + "::" + t
  }


  // --------------------------------------------------------------------------
  // Render type expression

  def writeAlignedCall(w: IndentWriter, call: String, params: Seq[Field], end: String, f: Field => String): IndentWriter =
    writeAlignedCall(w, call, params, ",", end, f)

  def writeAlignedCall(w: IndentWriter, call: String, params: Seq[Field], delim: String, end: String, f: Field => String): IndentWriter = {
    w.w(call)
    val skipFirst = new SkipFirst
    params.foreach(p => {
      skipFirst {
        w.wl(delim);
        w.w(" " * call.length())
      }
      w.w(f(p))
    })
    w.w(end)
  }

  def writeAlignedObjcCall(w: IndentWriter, call: String, params: Seq[Field], end: String, f: Field => (String, String)) = {
    w.w(call)
    val skipFirst = new SkipFirst
    params.foreach(p => {
      val (name, value) = f(p)
      skipFirst {
        w.wl;
        w.w(" " * math.max(0, call.length() - name.length));
        w.w(name)
      }
      w.w(":" + value)
    })
    w.w(end)
  }

  def writeEnumOptionNone(w: IndentWriter, e: Enum, ident: IdentConverter, marshal: Marshal) {
    for (o <- e.options.find(_.specialFlag.contains(Enum.SpecialFlag.NoFlags))) {
      writeDoc(w, o.doc)
      marshal.deprecatedAnnotation(o.deprecated).foreach(w.wl)
      w.wl(ident(o.ident.name) + " = 0,")
    }
  }

  def writeEnumOptions(w: IndentWriter, e: Enum, ident: IdentConverter, marshal: Marshal) {
    var shift = 0
    for (o <- normalEnumOptions(e)) {
      writeDoc(w, o.doc)
      if (o.value != None) {
        val constValue = o.value match {
          case Some(i) => " = " + i + ","
        }
        w.w(ident(o.ident.name) + " ")
        marshal.deprecatedAnnotation(o.deprecated).foreach(w.w)
        w.wl((s"$constValue"))
      } else {
        w.w(ident(o.ident.name) + " ")
        marshal.deprecatedAnnotation(o.deprecated).foreach(w.w)
        // w.wl((s"$constValue"))
        w.wl((if (e.flags) s" = 1 << $shift" else "") + ",")
        shift += 1
      }
    }
  }

  def normalEnumOptions(e: Enum) = e.options.filter(_.specialFlag == None)

  def writeEnumOptionAll(w: IndentWriter, e: Enum, ident: IdentConverter, marshal: Marshal) {
    for (o <- e.options.find(_.specialFlag == Some(Enum.SpecialFlag.AllFlags))) {
      writeDoc(w, o.doc)
      w.w(ident(o.ident.name))
      marshal.deprecatedAnnotation(o.deprecated).foreach(w.w)
      w.w(" = ")
      w.w(normalEnumOptions(e).map(o => ident(o.ident.name)).fold("0")((acc, o) => acc + " | " + o))
      w.wl(",")
    }
  }

  def writeMethodDoc(w: IndentWriter, method: Interface.Method, ident: IdentConverter) {
    val paramReplacements = method.params.map(p => (s"\\b${Regex.quote(p.ident.name)}\\b", s"${ident(p.ident.name)}"))
    val newDoc = Doc(method.doc.lines.map(l => {
      paramReplacements.foldLeft(l)((line, rep) =>
        line.replaceAll(rep._1, rep._2))
    }))
    writeDoc(w, newDoc)
  }

  def writeDoc(w: IndentWriter, doc: Doc) {
    doc.lines.length match {
      case 0 =>
      case 1 =>
        w.wl(s"/**${doc.lines.head} */")
      case _ =>
        w.wl("/**")
        doc.lines.foreach(l => w.wl(s" *$l"))
        w.wl(" */")
    }
  }

  // --------------------------------------------------------------------------

  protected def appendToFile(folder: File, fileName: String, f: IndentWriter => Unit): Unit = {
    val file = new File(folder, fileName)

    val fout = new FileOutputStream(file, true)
    try {
      val out = new OutputStreamWriter(fout, "UTF-8")
      f(new IndentWriter(out))
      out.flush()
    }
    finally {
      fout.close()
    }
  }

  protected def createFileOnce(folder: File, fileName: String, f: IndentWriter => Unit) {
    val file = new File(folder, fileName)
    val cp = file.getCanonicalPath
    Generator.writtenFiles.put(cp.toLowerCase, cp) match {
      case Some(existing) => return
      case _ =>
    }

    if (spec.outFileListWriter.isDefined) {
      spec.outFileListWriter.get.write(new File(folder, fileName).getPath + "\n")
    }
    if (spec.skipGeneration) {
      return
    }

    val fout = new FileOutputStream(file)
    try {
      val out = new OutputStreamWriter(fout, "UTF-8")
      f(new IndentWriter(out))
      out.flush()
    }
    finally {
      fout.close()
    }
  }
}
