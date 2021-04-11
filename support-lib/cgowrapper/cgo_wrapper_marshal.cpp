//
//  cgo_wrapper_marshal.cpp
//  demo
//
//  Created by Hien Nguyen on 13/01/2021.
//

#include "cgo_wrapper_marshal.hpp"

cgo__string DjinniString::from_cpp(const std::string & str) {
    return {str.length(), str.c_str()};
}

std::string DjinniString::to_cpp(const cgo__string & str) {
    return std::string(str.str, str.length);
}

void free_cgo_string(cgo__string * ptr) {
    delete ptr;
}

cgo__binary DjinniBinary::from_cpp(const std::vector<uint8_t> & data) {
    return {data.size(), std::move(data.data())};
}

std::vector<uint8_t> DjinniBinary::to_cpp(const cgo__binary & str) {
    auto result = std::vector<uint8_t>(str.data, str.data + str.length);
    // free cgo_binary
    return result;
}

void free_cgo_binary(cgo__binary * ptr) {
    delete ptr;
}
