// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from example.djinni

#include "NativeRc.hpp"  // my header
#include "JNIMarshal+Json.h"
#include "Marshal.hpp"
#include "NativeEn.hpp"
#include "NativeHien.hpp"

namespace djinni_generated {

NativeRc::NativeRc() = default;

NativeRc::~NativeRc() = default;

auto NativeRc::fromCpp(JNIEnv* jniEnv, const CppType& c) -> ::djinni::LocalRef<JniType> {
    const auto& data = ::djinni::JniClass<NativeRc>::get();
    auto r = ::djinni::LocalRef<JniType>{jniEnv->NewObject(data.clazz.get(), data.jconstructor,
                                                           ::djinni::get(::djinni::I32::fromCpp(jniEnv, c.a)),
                                                           ::djinni::get(::djinni::I32::fromCpp(jniEnv, c.b)),
                                                           ::djinni::get(::djinni::U32::fromCpp(jniEnv, c.c)),
                                                           ::djinni::get(::djinni::String::fromCpp(jniEnv, c.d)),
                                                           ::djinni::get(::djinni::List<::djinni::I16>::fromCpp(jniEnv, c.list_16)),
                                                           ::djinni::get(::djinni::List<::djinni::I32>::fromCpp(jniEnv, c.list)),
                                                           ::djinni::get(::djinni::List<::djinni::I8>::fromCpp(jniEnv, c.list8)),
                                                           ::djinni::get(::djinni::List<::djinni_generated::NativeHien>::fromCpp(jniEnv, c.list_hien)),
                                                           ::djinni::get(::djinni::Optional<std::optional, ::djinni::U16>::fromCpp(jniEnv, c.o)),
                                                           ::djinni::get(::djinni::Optional<std::optional, ::djinni_generated::NativeHien>::fromCpp(jniEnv, c.o1)),
                                                           ::djinni::get(::djinni_generated::NativeEn::fromCpp(jniEnv, c.e1)),
                                                           ::djinni::get(::djinni::Optional<std::optional, ::djinni_generated::NativeEn>::fromCpp(jniEnv, c.e2)))};
    ::djinni::jniExceptionCheck(jniEnv);
    return r;
}

auto NativeRc::toCpp(JNIEnv* jniEnv, JniType j) -> CppType {
    ::djinni::JniLocalScope jscope(jniEnv, 13);
    assert(j != nullptr);
    const auto& data = ::djinni::JniClass<NativeRc>::get();
    return {::djinni::I32::toCpp(jniEnv, jniEnv->GetIntField(j, data.field_mA)),
            ::djinni::I32::toCpp(jniEnv, jniEnv->GetIntField(j, data.field_mB)),
            ::djinni::U32::toCpp(jniEnv, jniEnv->GetLongField(j, data.field_mC)),
            ::djinni::String::toCpp(jniEnv, (jstring)jniEnv->GetObjectField(j, data.field_mD)),
            ::djinni::List<::djinni::I16>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mList16)),
            ::djinni::List<::djinni::I32>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mList)),
            ::djinni::List<::djinni::I8>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mList8)),
            ::djinni::List<::djinni_generated::NativeHien>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mListHien)),
            ::djinni::Optional<std::optional, ::djinni::U16>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mO)),
            ::djinni::Optional<std::optional, ::djinni_generated::NativeHien>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mO1)),
            ::djinni_generated::NativeEn::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mE1)),
            ::djinni::Optional<std::optional, ::djinni_generated::NativeEn>::toCpp(jniEnv, jniEnv->GetObjectField(j, data.field_mE2))};
}

}  // namespace djinni_generated