// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from enum.djinni

#pragma once

#include <atomic>
#include <experimental/optional>
#include "enum_usage_record.hpp"
#ifdef __cplusplus
extern "C" {
#endif

#include "dh__map_enum_color_enum_color.h"

#ifdef __cplusplus
}
#endif
struct DjinniMapEnumColorEnumColor {
    static djinni::Handle<DjinniObjectHandle> fromCpp(const std::unordered_map<::testsuite::color, ::testsuite::color> & dc);
    static std::unordered_map<::testsuite::color, ::testsuite::color> toCpp(djinni::Handle<DjinniObjectHandle> dh);
    static djinni::Handle<DjinniOptionalObjectHandle>fromCpp(std::experimental::optional<std::unordered_map<::testsuite::color, ::testsuite::color>> dc);
    static std::experimental::optional<std::unordered_map<::testsuite::color, ::testsuite::color>> toCpp(djinni::Handle<DjinniOptionalObjectHandle> dh);
};
