package djinni

import djinni.ast._
import djinni.generatorTools._
import djinni.meta._

import scala.annotation.tailrec

class SwiftMarshal(spec: Spec) extends Marshal(spec) {
  override def typename(name: String, ty: TypeDef): String = idSwift.ty(name)

  override def typename(tm: MExpr): String = ???

  override def fqTypename(tm: MExpr): String = ???

  override def paramType(tm: MExpr): String = getSwiftBridgingType(tm).swift

  override def fqParamType(tm: MExpr): String = ???

  override def returnType(ret: Option[TypeRef]): String = ???

  override def fqReturnType(ret: Option[TypeRef]): String = ???

  override def fieldType(tm: MExpr): String = ???

  override def fqFieldType(tm: MExpr): String = getSwiftBridgingType(tm).swift

  override def deprecatedAnnotation(deprecated: Option[Deprecated]): Option[String] = {
    deprecated match {
      case Some(value) => Some(s"@available(*, deprecated, message: ${value.messages})")
      case None => None
    }
  }

  def getSwiftBridgingType(tm: MExpr): SwiftBridgingType = {

    def find(tm: MExpr, downcast: Boolean = false): SwiftBridgingType = {
      val base: SwiftBridgingType = tm.base match {
        case opaque: MOpaque => opaque match {
          case p: MPrimitive =>
            if (downcast)
              SwiftBridgingType(wrapper = "NSNumber", swift = p.swiftName, typeCastingOperator = " as ")
            else
              SwiftBridgingType(wrapper = p.swiftName, swift = p.swiftName)
          case meta.MString => SwiftBridgingType(wrapper = "String", swift = "String")
          case meta.MDate => SwiftBridgingType(wrapper = "Date", swift = "Date")
          case meta.MBinary => SwiftBridgingType(wrapper = "Data", swift = "Data")
          case meta.MOptional =>
            // recursive to find the correct arg type
            val arg = tm.args.head

            val downcast = arg.base match {
              case opaque: MOpaque => opaque match {
                // Swift primitive can not be represent in Objc -> We need to uses the downcast value.
                case _: MPrimitive => true
                case _ => false
              }
              case d: MDef => d.defType match {
                case meta.DEnum =>
                  true
                case _ => false
              }
              case _ => false
            }

            val bridgingType = find(arg, downcast)
            SwiftBridgingType(wrapper = s"${bridgingType.wrapper}?", swift = s"${bridgingType.swift}?",
              downcast = downcast,
              typeCastingOperator = bridgingType.typeCastingOperator)
          case meta.MList =>
            val arg = tm.args.head

            val downcast = arg.base match {
              case d: MDef => d.defType match {
                case meta.DEnum =>
                  true
                case _ => false
              }
              case _ => false
            }

            val bridgingType = find(arg, downcast)
            SwiftBridgingType(wrapper = s"Array<${bridgingType.wrapper}>", swift = s"Array<${bridgingType.swift}>",
                              downcast = downcast)
          case meta.MSet =>
            val arg = tm.args.head

            val downcast = arg.base match {
              case d: MDef => d.defType match {
                case meta.DEnum => true
                case _ => false
              }
              case _ => false
            }

            val bridgingType = find(arg, downcast)

            SwiftBridgingType(wrapper = s"Set<${bridgingType.wrapper}>", swift = s"Set<${bridgingType.swift}>", downcast = downcast)
          case meta.MMap =>
            val key = tm.args.head
            val value = tm.args.last

            val keyDowncast = key.base match {
              case d: MDef => d.defType match {
                case meta.DEnum => true
                case _ => false
              }
              case _ => false
            }

            val valueDowncast = value.base match {
              case d: MDef => d.defType match {
                case meta.DEnum => true
                case _ => false
              }
              case _ => false
            }

            val keyBridgingType = find(key, keyDowncast)
            val valueBridgingType = find(value, valueDowncast)

            val bridgingType = s"Dictionary<${
              if (keyDowncast) keyBridgingType.wrapper
              else keyBridgingType.swift
            }, ${
              if (valueDowncast) valueBridgingType.wrapper
              else valueBridgingType.swift
            }>"

            SwiftBridgingType(
              wrapper = bridgingType,
              swift = s"Dictionary<${keyBridgingType.swift}, ${valueBridgingType.swift}>",
              downcast = keyDowncast | valueDowncast
            )

          case meta.MJson =>
            val bridgingType = s"Dictionary<String, Any>"
            SwiftBridgingType(wrapper = bridgingType, swift = bridgingType)
        }
        case d: MDef => d.defType match {
          case meta.DEnum =>
            if (downcast)
              SwiftBridgingType(wrapper = "NSNumber", swift = idSwift.ty(d.name), typeCastingOperator = "?.rawValue as ")
            else
              SwiftBridgingType(wrapper = idSwift.ty(d.name), swift = idSwift.ty(d.name))
          case meta.DRecord =>
            val objcWrapper = idObjc.ty(d.name)
            val swift = idSwift.ty(d.name)
            SwiftBridgingType(wrapper = objcWrapper, swift = swift)
          case meta.DInterface => throw new AssertionError("Parameter should not happen at Obj-C top level")
        }
        case MExtern(name, numParams, defType, body, cpp, objc, objcpp, java, jni) => throw new AssertionError("Parameter should not happen at Obj-C top level")
        case _ => throw new AssertionError("Parameter should not happen at Obj-C top level")
      }
      base
    }

    find(tm = tm)
  }

  def toObjc(tm: MExpr, expr: String): String = {
    s"${getHelperName(tm)}.toObjc(e: $expr)"
  }

  def fromObjc(tm: MExpr, expr: String): String = {
    s"${getHelperName(tm)}.fromObjc(e: $expr)"
  }

  override protected def withNs(namespace: Option[String], t: String): String = namespace match {
    case None => t
    case Some("") => t
    case Some(s) => s + "." + t
  }

  def swiftHelper(expr: String): String = {
    s"DjinniSwift.$expr"
  }

  def getHelperName(tm: MExpr): String = {
    def findHelperName(tm: MExpr): String = {
      val base: String = tm.base match {
        case d: MDef => d.defType match {
          case DEnum => s"${swiftHelper(s"Enum<${idSwift.ty(d.name)}>")}"
          case DRecord => s"${idSwift.ty(d.name)}"
          case _ => throw new AssertionError("unreachable")
        }
        case e: MExtern => e.objcpp.translator
        case o => withNs(None, o match {
          case p: MPrimitive => p.idlName match {
            case "i8" => swiftHelper("I8")
            case "i16" => swiftHelper("I16")
            case "i32" => swiftHelper("I32")
            case "i64" => swiftHelper("I64")
            case "f32" => swiftHelper("F32")
            case "f64" => swiftHelper("F64")
            case "bool" => swiftHelper("Bool")
            case "u8" => swiftHelper("U8")
            case "u16" => swiftHelper("U16")
            case "u32" => swiftHelper("U32")
          }
          case MOptional =>
            val arg = tm.args.head
            findHelperName(arg)
          case MBinary => "Binary"
          case MDate => "Date"
          case MString => if (spec.cppUseWideStrings) "WString" else "String"
          case MList =>
            val arg = tm.args.head
            val helper = findHelperName(arg)
            s"List<$helper>"
          case MSet =>
            val arg = tm.args.head
            val helper = findHelperName(arg)
            s"SetHelper<$helper>"
          case MMap =>
            val key = tm.args.head
            val value = tm.args.last
            val keyHelper = findHelperName(key)
            val valueHelper = findHelperName(value)
            s"${swiftHelper(s"Map<$keyHelper, $valueHelper>")}"
          case MJson => "Json"
          case _: MDef => throw new AssertionError("unreachable")
          case _: MExtern => throw new AssertionError("unreachable")
          case _: MParam => throw new AssertionError("not applicable")
        })
      }
      base
    }

    findHelperName(tm)
  }
}