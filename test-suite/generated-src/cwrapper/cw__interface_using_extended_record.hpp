// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from extended_record.djinni

#pragma once

#include <atomic>
#include <experimental/optional>
#include "interface_using_extended_record.hpp"
#ifdef __cplusplus
extern "C" {
#endif

#include "cw__interface_using_extended_record.h"

#ifdef __cplusplus
}
#endif
struct DjinniWrapperInterfaceUsingExtendedRecord final {
    DjinniWrapperInterfaceUsingExtendedRecord(std::shared_ptr<::testsuite::InterfaceUsingExtendedRecord>wo): wrapped_obj(wo) {};

    static std::shared_ptr<::testsuite::InterfaceUsingExtendedRecord> get(djinni::Handle<DjinniWrapperInterfaceUsingExtendedRecord> dw);
    static djinni::Handle<DjinniWrapperInterfaceUsingExtendedRecord> wrap(std::shared_ptr<::testsuite::InterfaceUsingExtendedRecord> obj);

    const std::shared_ptr<::testsuite::InterfaceUsingExtendedRecord> wrapped_obj;
    std::atomic<size_t> ref_count {1};
};
