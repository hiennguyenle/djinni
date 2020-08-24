// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from example.djinni

#pragma once

#include "djinni_support.hpp"
#include "hien.hpp"

namespace djinni_generated {

class NativeHien final {
public:
    using CppType = ::textsort::Hien;
    using JniType = jobject;

    using Boxed = NativeHien;

    ~NativeHien();

    static CppType toCpp(JNIEnv* jniEnv, JniType j);
    static ::djinni::LocalRef<JniType> fromCpp(JNIEnv* jniEnv, const CppType& c);

private:
    NativeHien();
    friend ::djinni::JniClass<NativeHien>;

    const ::djinni::GlobalRef<jclass> clazz { ::djinni::jniFindClass("com/dropbox/textsort/Hien") };
    const jmethodID jconstructor { ::djinni::jniGetMethodID(clazz.get(), "<init>", "(Ljava/lang/String;Ljava/lang/String;)V") };
    const jfieldID field_mHien { ::djinni::jniGetFieldID(clazz.get(), "mHien", "Ljava/lang/String;") };
    const jfieldID field_mHien1 { ::djinni::jniGetFieldID(clazz.get(), "mHien1", "Ljava/lang/String;") };
};

}  // namespace djinni_generated