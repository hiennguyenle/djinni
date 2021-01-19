// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from example.djinni

#import "Rc+Private.h"
#import "En+Private.h"
#import "Hien+Private.h"
#import <Djinni/DJIMarshal+Json.h>
#import <Djinni/DJIMarshal+Private.h>
#include <cassert>

namespace djinni_generated {

auto RcHelper::toCpp(ObjcType obj) -> CppType
{
    assert(obj);
    return {::djinni::I32::toCpp(obj.__djinni__objc_a),
            ::djinni::I32::toCpp(obj.__djinni__objc_b),
            ::djinni::U32::toCpp(obj.__djinni__objc_c),
            ::djinni::String::toCpp(obj.__djinni__objc_d),
            ::djinni::List<::djinni::I16>::toCpp(obj.__djinni__objc_list16),
            ::djinni::List<::djinni::I32>::toCpp(obj.__djinni__objc_list),
            ::djinni::List<::djinni::I8>::toCpp(obj.__djinni__objc_list8),
            ::djinni::List<::djinni_generated::HienHelper>::toCpp(obj.__djinni__objc_listHien),
            ::djinni::Optional<std::optional, ::djinni::U16>::toCpp(obj.__djinni__objc_o),
            ::djinni::Optional<std::optional, ::djinni_generated::HienHelper>::toCpp(obj.__djinni__objc_o1),
            ::djinni::Enum<::cpp_generated::En, En>::toCpp(obj.__djinni__objc_e1),
            ::djinni::Optional<std::optional, ::djinni::Enum<::cpp_generated::En, En>>::toCpp(obj.__djinni__objc_e2)};
}

auto RcHelper::fromCpp(const CppType& cpp) -> ObjcType
{
    return [Rc initWithA:(::djinni::I32::fromCpp(cpp.a))
                       b:(::djinni::I32::fromCpp(cpp.b))
                       c:(::djinni::U32::fromCpp(cpp.c))
                       d:(::djinni::String::fromCpp(cpp.d))
                  list16:(::djinni::List<::djinni::I16>::fromCpp(cpp.list_16))
                    list:(::djinni::List<::djinni::I32>::fromCpp(cpp.list))
                   list8:(::djinni::List<::djinni::I8>::fromCpp(cpp.list8))
                listHien:(::djinni::List<::djinni_generated::HienHelper>::fromCpp(cpp.list_hien))
                       o:(::djinni::Optional<std::optional, ::djinni::U16>::fromCpp(cpp.o))
                      o1:(::djinni::Optional<std::optional, ::djinni_generated::HienHelper>::fromCpp(cpp.o1))
                      e1:(::djinni::Enum<::cpp_generated::En, En>::fromCpp(cpp.e1))
                      e2:(::djinni::Optional<std::optional, ::djinni::Enum<::cpp_generated::En, En>>::fromCpp(cpp.e2))];
}

}  // namespace djinni_generated
