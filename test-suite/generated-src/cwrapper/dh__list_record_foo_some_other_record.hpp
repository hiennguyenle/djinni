// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from foo_containers.djinni

#pragma once

#include <atomic>
#include <experimental/optional>
#include "foo_containers_record.hpp"
#ifdef __cplusplus
extern "C" {
#endif

#include "dh__list_record_foo_some_other_record.h"

#ifdef __cplusplus
}
#endif
struct DjinniListRecordFooSomeOtherRecord {
    static djinni::Handle<DjinniObjectHandle> fromCpp(const std::vector<::testsuite::FooSomeOtherRecord> & dc);
    static std::vector<::testsuite::FooSomeOtherRecord> toCpp(djinni::Handle<DjinniObjectHandle> dh);
    static djinni::Handle<DjinniOptionalObjectHandle>fromCpp(std::experimental::optional<std::vector<::testsuite::FooSomeOtherRecord>> dc);
    static std::experimental::optional<std::vector<::testsuite::FooSomeOtherRecord>> toCpp(djinni::Handle<DjinniOptionalObjectHandle> dh);
};
