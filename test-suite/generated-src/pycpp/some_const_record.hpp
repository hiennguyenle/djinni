// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from foo_constants.djinni

#pragma once

#include <cstdint>
#include <utility>

namespace testsuite {

struct SomeConstRecord final {
    int16_t number1;
    int16_t number2;

    SomeConstRecord(int16_t number1_,
                    int16_t number2_)
    : number1(std::move(number1_))
    , number2(std::move(number2_))
    {}
};

}  // namespace testsuite
