package djinni

import djinni.ast._
import djinni.generatorTools._
import djinni.meta._

class CgoWrapperMarshal(spec: Spec) extends Marshal(spec) { // modeled(pretty much copied) after CppMarshal, not fully C-like
    val cppMarshal = new CppMarshal(spec)
    val djinniWrapper = "Djinni"
    val djinniObjectHandle = "DjinniObjectHandle"
    val cgo = "cgo__" // prefix for c files containing djinni helpers for records

    override def typename(tm: MExpr): String = throw new NotImplementedError()

    override def typename(name: String, ty: TypeDef): String = ty match {
        case e: Enum => idCpp.enumType(name)
        case i: Interface => idCpp.ty(name)
        case r: Record => idCpp.ty(name)
    }

    override def fqTypename(tm: MExpr): String = throw new NotImplementedError()

    override def paramType(tm: MExpr): String = throw new NotImplementedError()
    override def fqParamType(tm: MExpr): String = throw new NotImplementedError()

    override def returnType(ret: Option[TypeRef]): String = throw new NotImplementedError()

    override def fqReturnType(ret: Option[TypeRef]): String = throw new NotImplementedError()

    override def fieldType(tm: djinni.meta.MExpr): String = toCwrapperType(tm, false)

    override def fqFieldType(tm: djinni.meta.MExpr): String = throw new NotImplementedError()

    def cReturnType(ret: Option[TypeRef], forHeader: Boolean = false): String = {
        if (ret.isEmpty) return "void"
        toCwrapperType(ret.get.resolved, forHeader)
    }

    def cArgDecl(args: Seq[String]) = {
        if (args.isEmpty) {
            // CWrapper headers need to be parsed as C.  `()` in C means "unspecified args" and triggers
            // -Wstrict-prototypes.  `(void)` means no args in C.  In C++ the two forms are equivalent.
            "(void)"
        } else {
            args.mkString("(", ", ", ")")
        }
    }
    def toCwrapperType(tm: MExpr, forHeader: Boolean): String = {
        def base(m: Meta): String = {
            val structPrefix = if (forHeader) "struct " else ""
            m match {
                case p: MPrimitive => p.cName
                case MDate => "uint64_t"
                case MString => structPrefix + cgo + "string"
                case MBinary => structPrefix + cgo + "binary"
                case MList => structPrefix + cgo + "list__" + base(tm.args.head.base)
                case MSet | MMap => structPrefix + djinniObjectHandle + " *"
                case MOptional => tm.args.head.base match {
                    case p: MPrimitive => p.cName + " *"
                    case MList | MSet | MMap => structPrefix + "DjinniOptionalObjectHandle *"
                    case d: MDef =>
                        d.defType match {
                            case DRecord | DEnum => s"$cgo${d.name} *"
                            case _ => base(tm.args.head.base)
                        }
                    case _ => base(tm.args.head.base)
                }
                case d: MDef =>
                    d.defType match {
                        case DEnum | DRecord => cgo + d.name
                        case DInterface => cgo + d.name + " *"

                    }
                case p: MParam => idCpp.typeParam(p.name)
                case e: MExtern => throw new NotImplementedError()
            }
        }

        def expr(tm: MExpr): String = {
            base(tm.base)
        }

        expr(tm)
    }
    
    
    def hReferences(m: Meta, exclude: String): Seq[SymbolReference] = m match {
        case p: MPrimitive => p.idlName match {
            case "i8" | "u8" | "i16" | "u16" | "u32" | "i32" | "i64" => List(ImportRef("<stdint.h>"))
            case "bool" => List(ImportRef("<stdbool.h>"))
            case _ => List()
        }
        case MDate => List()
        case MBinary => List()
        case MOptional => List()
        case d: MDef => d.defType match {
            case DInterface => List(ImportRef(q(cgo + d.name + ".h")))
            case DRecord => List(ImportRef(q(cgo + d.name + ".h")))
            case DEnum => List(ImportRef(q(cgo + d.name + ".h")))
        }
        case e: MExtern => throw new NotImplementedError()
        case _ => List()
    }

    def hppReferences(m: Meta, exclude: String): Seq[SymbolReference] = m match {
        case p: MPrimitive => p.idlName match {
            case "i8" | "u8" | "i16" | "u16" | "u32" | "i32" | "i64" => List(ImportRef("<stdint.h>"))
            case "bool" => List(ImportRef("<stdbool.h>"))
            case _ => List()
        }
        case MDate => List(ImportRef("<chrono>"))
        case MString => List(ImportRef("<string>"))
        case MBinary => List(ImportRef("<vector>"), ImportRef("<stdint.h>"))
        case MOptional => List(ImportRef(spec.cppOptionalHeader))
        case d: MDef => d.defType match {
            case DInterface => List()
            case DRecord =>
                List(ImportRef(q(cgo + d.name + ".hpp")), ImportRef(q(d.name + ".hpp")))
            case DEnum => List(ImportRef(q(d.name + ".hpp")), ImportRef(q(cgo + d.name + ".hpp")))
        }
        case e: MExtern => throw new NotImplementedError()
        case _ => List()
    }

    def cgoWrapperType(tm: MExpr): String = {
        def find(m: Meta): String = m match {
            case p: MPrimitive => p.cName
            case MString => cgo + "string"
            case MBinary => cgo + "binary"
            case d: MDef =>
                d.defType match {
                    case DRecord | DEnum => cgo + d.name
                    case DInterface => "interface_" + d.name
                }
            case p: MParam => idCpp.typeParam(p.name)
            case e: MExtern => "extern"
            case MOptional => tm.args.head.base match {
                case mp: MPrimitive => "boxed"
                case _ => "optional"
            }
            case MList =>
                val field_name = find(tm.args.head.base)
                cgo + "list__" + field_name
            case _ => m.asInstanceOf[MOpaque].idlName
        }

        find(tm.base)
    }

    def cHeader(ident: Ident): String = {
         q(cgo + ident.name + ".h")
    }

    def cppHeader(ident: Ident): String = {
         q(cgo + ident.name + ".hpp")
    }

    override def fromCpp(tm: MExpr, expr: String): String = {
        def base(m: Meta): String = m match {
            case opaque: MOpaque => opaque match {
                case p: MPrimitive =>s"std::move($expr)"
                case meta.MString => s"DjinniString::from_cpp($expr)"
                case meta.MList =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgoWrapperType(tm))
                    s"$cppHelperClass::from_cpp($expr)"
                case meta.MDate => throw new NotImplementedError()
                case meta.MBinary => s"DjinniBinary::from_cpp($expr)"
                case meta.MOptional =>
                    val baseField = tm.args.head
                    val cgoReturnType = cgoWrapperType(baseField)
                    s"DjinniCgoOptional<$cgoReturnType>::from_cpp(${base(baseField.base)})"
                case meta.MSet => throw new NotImplementedError()
                case meta.MMap => throw new NotImplementedError()
                case meta.MJson => throw new NotImplementedError()
            }
            case MParam(name) => throw new NotImplementedError()
            case d: MDef => d.defType match {
                case meta.DEnum =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgo + d.name)
                    s"$cppHelperClass::from_cpp($expr)"
                case meta.DInterface => throw new NotImplementedError()
                case meta.DRecord =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgo + d.name)
                    s"$cppHelperClass::from_cpp($expr)"
            }
            case e: MExtern => throw new NotImplementedError()
        }

        base(tm.base)
    }

    override def toCpp(tm: MExpr, expr: String): String = {
        def base(m: Meta): String = m match {
            case opaque: MOpaque => opaque match {
                case p: MPrimitive =>s"std::move($expr)"
                case meta.MString => s"DjinniString::to_cpp($expr)"
                case meta.MList =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgoWrapperType(tm))
                    s"$cppHelperClass::to_cpp($expr)"
                case meta.MDate => throw new NotImplementedError()
                case meta.MBinary => s"DjinniBinary::to_cpp($expr)"
                case meta.MOptional =>
                    val baseField = tm.args.head.base
                    baseField match {
                        case p: MPrimitive =>s"DjinniCgoOptional<${p.cName}>::to_cpp(${base(baseField)})"
                        case _ => s"${base(baseField)}"
                    }
                case meta.MSet => throw new NotImplementedError()
                case meta.MMap => throw new NotImplementedError()
                case meta.MJson => throw new NotImplementedError()
            }
            case MParam(name) => throw new NotImplementedError()
            case d: MDef => d.defType match {
                case meta.DEnum =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgo + d.name)
                    s"$cppHelperClass::to_cpp($expr)"
                case meta.DInterface => throw new NotImplementedError()
                case meta.DRecord =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgo + d.name)
                    s"$cppHelperClass::to_cpp($expr)"
            }
            case e: MExtern => throw new NotImplementedError()
        }
        base(tm.base)
    }

    def free_memory(tm: MExpr, expr: String): Option[String] = {
        def base(m: Meta, pointer: Boolean = false): Option[String] = m match {
            case opaque: MOpaque => opaque match {
                case meta.MString => Option(s"free_cgo_string(&($expr))")
                case meta.MList =>
                    val list_name = s"${cgoWrapperType(tm)}"
                    Option(s"${list_name}__delete(&($expr))")
                case meta.MDate => throw new NotImplementedError()
                case meta.MBinary => Option(s"free_cgo_binary(&($expr))")
                case meta.MOptional =>
                    val baseField = tm.args.head.base
                    baseField match {
                        case p: MPrimitive => Option(s"delete $expr")
                        case _ => base(baseField, pointer = true)
                    }
                case p: MPrimitive => null
                case meta.MSet => throw new NotImplementedError()
                case meta.MMap => throw new NotImplementedError()
                case meta.MJson => throw new NotImplementedError()
            }
            case MParam(name) => throw new NotImplementedError()
            case d: MDef => d.defType match {
                case meta.DRecord =>
                    val cppHelperClass = s"$djinniWrapper" + idCpp.typeParam(cgo + d.name)
                    val ptr = if (pointer) {
                        s"$expr"
                    } else {
                        s"&($expr)"
                    }
                    Option(s"${cgo + d.name}__delete($ptr)")
                case _ => null
            }
            case e: MExtern => throw new NotImplementedError()
        }
        base(tm.base)
    }
}