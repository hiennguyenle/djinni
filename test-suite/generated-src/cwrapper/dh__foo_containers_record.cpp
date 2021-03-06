// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from foo_containers.djinni

#include <iostream> // for debugging
#include <cassert>
#include "wrapper_marshal.hpp"
#include "foo_containers_record.hpp"

#include "dh__foo_containers_record.hpp"
#include "dh__foo_some_other_record.hpp"
#include "dh__list_binary.hpp"
#include "dh__list_date.hpp"
#include "dh__list_int32_t.hpp"
#include "dh__list_list_string.hpp"
#include "dh__list_optional_binary.hpp"
#include "dh__list_record_foo_some_other_record.hpp"
#include "dh__list_string.hpp"
#include "dh__map_boxed_int32_t_set_string.hpp"
#include "dh__map_int8_t_list_date.hpp"
#include "dh__map_int8_t_set_string.hpp"
#include "dh__map_optional_string_optional_string.hpp"
#include "dh__map_string_int32_t.hpp"
#include "dh__map_string_string.hpp"
#include "dh__set_optional_string.hpp"
#include "dh__set_string.hpp"
#include <chrono>
#include <experimental/optional>
#include <vector>

static void(*s_py_callback_foo_containers_record___delete)(DjinniRecordHandle * );
void foo_containers_record_add_callback___delete(void(* ptr)(DjinniRecordHandle * )) {
    s_py_callback_foo_containers_record___delete = ptr;
}

void foo_containers_record___delete(DjinniRecordHandle * drh) {
    s_py_callback_foo_containers_record___delete(drh);
}
void optional_foo_containers_record___delete(DjinniOptionalRecordHandle * drh) {
    s_py_callback_foo_containers_record___delete((DjinniRecordHandle *) drh); // can't static cast, find better way
}
static DjinniOptionalObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f1)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f1(DjinniOptionalObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f1 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f2)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f2(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f2 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f3)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f3(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f3 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f4)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f4(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f4 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f5)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f5(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f5 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f6)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f6(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f6 = ptr;
}

static DjinniOptionalObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f7)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f7(DjinniOptionalObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f7 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f8)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f8(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f8 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f9)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f9(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f9 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f10)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f10(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f10 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f11)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f11(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f11 = ptr;
}

static DjinniOptionalObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f12)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f12(DjinniOptionalObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f12 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f13)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f13(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f13 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f14)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f14(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f14 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f15)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f15(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f15 = ptr;
}

static DjinniObjectHandle * ( * s_py_callback_foo_containers_record_get_foo_containers_record_f16)(DjinniRecordHandle *);

void foo_containers_record_add_callback_get_foo_containers_record_f16(DjinniObjectHandle *( * ptr)(DjinniRecordHandle *)) {
    s_py_callback_foo_containers_record_get_foo_containers_record_f16 = ptr;
}

static DjinniRecordHandle * ( * s_py_callback_foo_containers_record_python_create_foo_containers_record)(DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *);

void foo_containers_record_add_callback_python_create_foo_containers_record(DjinniRecordHandle *( * ptr)(DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniOptionalObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *, DjinniObjectHandle *)) {
    s_py_callback_foo_containers_record_python_create_foo_containers_record = ptr;
}

djinni::Handle<DjinniRecordHandle> DjinniFooContainersRecord::fromCpp(const ::testsuite::FooContainersRecord& dr) {
    auto  _field_optional_list_int = DjinniListInt32T::fromCpp(dr.optional_list_int);
    auto  _field_list_int = DjinniListInt32T::fromCpp(dr.list_int);
    auto  _field_list_binary = DjinniListBinary::fromCpp(dr.list_binary);
    auto  _field_list_optional_binary = DjinniListOptionalBinary::fromCpp(dr.list_optional_binary);
    auto  _field_list_list_string = DjinniListListString::fromCpp(dr.list_list_string);
    auto  _field_list_record = DjinniListRecordFooSomeOtherRecord::fromCpp(dr.list_record);
    auto  _field_optional_map_string_int = DjinniMapStringInt32T::fromCpp(dr.optional_map_string_int);
    auto  _field_map_string_int = DjinniMapStringInt32T::fromCpp(dr.map_string_int);
    auto  _field_map_string_string = DjinniMapStringString::fromCpp(dr.map_string_string);
    auto  _field_map_optional_string_optional_string = DjinniMapOptionalStringOptionalString::fromCpp(dr.map_optional_string_optional_string);
    auto  _field_map_int_list_date = DjinniMapInt8TListDate::fromCpp(dr.map_int_list_date);
    auto  _field_optional_set_string = DjinniSetString::fromCpp(dr.optional_set_string);
    auto  _field_set_string = DjinniSetString::fromCpp(dr.set_string);
    auto  _field_set_optional_string = DjinniSetOptionalString::fromCpp(dr.set_optional_string);
    auto  _field_map_int_set_string = DjinniMapInt8TSetString::fromCpp(dr.map_int_set_string);
    auto  _field_map_optional_int_set_string = DjinniMapBoxedInt32TSetString::fromCpp(dr.map_optional_int_set_string);

    djinni::Handle<DjinniRecordHandle> _aux(
        s_py_callback_foo_containers_record_python_create_foo_containers_record(
            _field_optional_list_int.release(),
            _field_list_int.release(),
            _field_list_binary.release(),
            _field_list_optional_binary.release(),
            _field_list_list_string.release(),
            _field_list_record.release(),
            _field_optional_map_string_int.release(),
            _field_map_string_int.release(),
            _field_map_string_string.release(),
            _field_map_optional_string_optional_string.release(),
            _field_map_int_list_date.release(),
            _field_optional_set_string.release(),
            _field_set_string.release(),
            _field_set_optional_string.release(),
            _field_map_int_set_string.release(),
            _field_map_optional_int_set_string.release()),
        foo_containers_record___delete);
    return _aux;
}

::testsuite::FooContainersRecord DjinniFooContainersRecord::toCpp(djinni::Handle<DjinniRecordHandle> dh) {
    djinni::Handle<DjinniOptionalObjectHandle> _field_optional_list_int(s_py_callback_foo_containers_record_get_foo_containers_record_f1(dh.get()), optional_list_int32_t___delete);
    djinni::Handle<DjinniObjectHandle> _field_list_int(s_py_callback_foo_containers_record_get_foo_containers_record_f2(dh.get()), list_int32_t___delete);
    djinni::Handle<DjinniObjectHandle> _field_list_binary(s_py_callback_foo_containers_record_get_foo_containers_record_f3(dh.get()), list_binary___delete);
    djinni::Handle<DjinniObjectHandle> _field_list_optional_binary(s_py_callback_foo_containers_record_get_foo_containers_record_f4(dh.get()), list_optional_binary___delete);
    djinni::Handle<DjinniObjectHandle> _field_list_list_string(s_py_callback_foo_containers_record_get_foo_containers_record_f5(dh.get()), list_list_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_list_record(s_py_callback_foo_containers_record_get_foo_containers_record_f6(dh.get()), list_record_foo_some_other_record___delete);
    djinni::Handle<DjinniOptionalObjectHandle> _field_optional_map_string_int(s_py_callback_foo_containers_record_get_foo_containers_record_f7(dh.get()), optional_map_string_int32_t___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_string_int(s_py_callback_foo_containers_record_get_foo_containers_record_f8(dh.get()), map_string_int32_t___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_string_string(s_py_callback_foo_containers_record_get_foo_containers_record_f9(dh.get()), map_string_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_optional_string_optional_string(s_py_callback_foo_containers_record_get_foo_containers_record_f10(dh.get()), map_optional_string_optional_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_int_list_date(s_py_callback_foo_containers_record_get_foo_containers_record_f11(dh.get()), map_int8_t_list_date___delete);
    djinni::Handle<DjinniOptionalObjectHandle> _field_optional_set_string(s_py_callback_foo_containers_record_get_foo_containers_record_f12(dh.get()), optional_set_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_set_string(s_py_callback_foo_containers_record_get_foo_containers_record_f13(dh.get()), set_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_set_optional_string(s_py_callback_foo_containers_record_get_foo_containers_record_f14(dh.get()), set_optional_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_int_set_string(s_py_callback_foo_containers_record_get_foo_containers_record_f15(dh.get()), map_int8_t_set_string___delete);
    djinni::Handle<DjinniObjectHandle> _field_map_optional_int_set_string(s_py_callback_foo_containers_record_get_foo_containers_record_f16(dh.get()), map_boxed_int32_t_set_string___delete);

    auto _aux = ::testsuite::FooContainersRecord(
        DjinniListInt32T::toCpp(std::move( _field_optional_list_int)),
        DjinniListInt32T::toCpp(std::move( _field_list_int)),
        DjinniListBinary::toCpp(std::move( _field_list_binary)),
        DjinniListOptionalBinary::toCpp(std::move( _field_list_optional_binary)),
        DjinniListListString::toCpp(std::move( _field_list_list_string)),
        DjinniListRecordFooSomeOtherRecord::toCpp(std::move( _field_list_record)),
        DjinniMapStringInt32T::toCpp(std::move( _field_optional_map_string_int)),
        DjinniMapStringInt32T::toCpp(std::move( _field_map_string_int)),
        DjinniMapStringString::toCpp(std::move( _field_map_string_string)),
        DjinniMapOptionalStringOptionalString::toCpp(std::move( _field_map_optional_string_optional_string)),
        DjinniMapInt8TListDate::toCpp(std::move( _field_map_int_list_date)),
        DjinniSetString::toCpp(std::move( _field_optional_set_string)),
        DjinniSetString::toCpp(std::move( _field_set_string)),
        DjinniSetOptionalString::toCpp(std::move( _field_set_optional_string)),
        DjinniMapInt8TSetString::toCpp(std::move( _field_map_int_set_string)),
        DjinniMapBoxedInt32TSetString::toCpp(std::move( _field_map_optional_int_set_string)));
    return _aux;
}

djinni::Handle<DjinniOptionalRecordHandle> DjinniFooContainersRecord::fromCpp(std::experimental::optional<::testsuite::FooContainersRecord> dc) {
    if (dc == std::experimental::nullopt) {
        return nullptr;
    }
    return djinni::optionals::toOptionalHandle(DjinniFooContainersRecord::fromCpp(std::move(* dc)), optional_foo_containers_record___delete);
}

std::experimental::optional<::testsuite::FooContainersRecord>DjinniFooContainersRecord::toCpp(djinni::Handle<DjinniOptionalRecordHandle> dh) {
     if (dh) {
        return std::experimental::optional<::testsuite::FooContainersRecord>(DjinniFooContainersRecord::toCpp(djinni::optionals::fromOptionalHandle(std::move(dh), foo_containers_record___delete)));
    }
    return std::experimental::nullopt;
}

