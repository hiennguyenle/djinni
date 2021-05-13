//
//  cgo_wrapper_marshal.cpp
//  demo
//
//  Created by Hien Nguyen on 13/01/2021.
//

#include "cgo_wrapper_marshal.hpp"
#include <string>

std::unique_ptr<cgo__string> DjinniString::from_cpp(const std::string & str) {
    
    size_t size = str.length();
    char * data = new char[size];
    std::memcpy(data, str.c_str(), size);
    cgo__string *cgo = new cgo__string({size, data});
    return std::unique_ptr<cgo__string>(std::move(cgo));
}

std::string DjinniString::to_cpp(const cgo__string & str) {
    return std::string(str.data, str.length);
}

std::string DjinniString::to_cpp(cgo__string * str) {
    return to_cpp(*str);
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

std::vector<uint8_t> DjinniBinary::to_cpp(cgo__binary * str) {
    return to_cpp(*str);
}

void free_cgo_binary(cgo__binary * ptr) {
    delete ptr;
}
