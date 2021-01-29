//
//  cgo_wrapper_marshal.hpp
//  demo
//
//  Created by Hien Nguyen on 13/01/2021.
//

#ifndef cgo_wrapper_marshal_hpp
#define cgo_wrapper_marshal_hpp

#include <stdio.h>
#include <string>
#include <vector>
#include <optional>
#include "cgo_wrapper_marshal.h"

struct DjinniString {
    static cgo__string from_cpp(const std::string & str);
    static std::string to_cpp(const cgo__string & str);
};

struct DjinniBinary {
    static cgo__binary from_cpp(const std::vector<uint8_t> & data);
    static std::vector<uint8_t> to_cpp(const cgo__binary & str);
};

template<typename T>
struct DjinniCgoOptional {
    
    static T* from_cpp(const std::optional<T> & cpp) {
        if (cpp.has_value()) {
            return const_cast<T *>(&(*cpp));
        }
        return NULL;
    }
    
    static std::optional<T> to_cpp(const T* cgo) {
        if (cgo == NULL) {
            return std::nullopt;
        }
        
        return std::optional<T>(*cgo);
    }
};

#endif /* cgo_wrapper_marshal_hpp */
