// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from client_interface.djinni

#include "NativeClientInterface.hpp"  // my header
#include "HString.hpp"
#include "NativeClientReturnedRecord.hpp"

namespace djinni_generated {

NativeClientInterface::NativeClientInterface() : djinni::JniInterface<ClientInterface, NativeClientInterface>() {}

NativeClientInterface::JavaProxy::JavaProxy(jobject obj) : JavaProxyCacheEntry(obj) {}

ClientReturnedRecord NativeClientInterface::JavaProxy::JavaProxy::get_record(const std::string & c_utf8string) {
    JNIEnv * const jniEnv = djinni::jniGetThreadEnv();
    djinni::JniLocalScope jscope(jniEnv, 10);
    djinni::LocalRef<jstring> j_utf8string(jniEnv, ::djinni::HString::toJava(jniEnv, c_utf8string));
    const auto & data = djinni::JniClass<::djinni_generated::NativeClientInterface>::get();
    djinni::LocalRef<jobject> jret(jniEnv, jniEnv->CallObjectMethod(getGlobalRef(), data.method_getRecord, j_utf8string.get()));
    djinni::jniExceptionCheck(jniEnv);
    return NativeClientReturnedRecord::fromJava(jniEnv, jret.get());
};

}  // namespace djinni_generated
