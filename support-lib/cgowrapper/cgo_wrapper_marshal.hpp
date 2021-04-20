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
#include <memory>
#include "cgo_wrapper_marshal.h"

struct DjinniString {
    static cgo__string from_cpp(const std::string & str);
    static std::string to_cpp(const cgo__string & str);
    static std::string to_cpp(cgo__string * str);
};

struct DjinniBinary {
    static cgo__binary from_cpp(const std::vector<uint8_t> & data);
    static std::vector<uint8_t> to_cpp(const cgo__binary & str);
    static std::vector<uint8_t> to_cpp(cgo__binary * str);
};

// Optional Primitives Template
template<class CppType>
class CgoPrimitive {
public:
    static std::optional<CppType> to_cpp(CppType *dopt) {
        if (dopt) {
            return std::make_optional<CppType>(std::move(*dopt));
        }
        return std::nullopt;
    }
    
    static std::unique_ptr<CppType> from_cpp(std::optional<CppType> copt) {
        if (!copt.has_value()){
            return nullptr;
        }
        return std::make_unique<CppType>(std::move(*copt));
    }
};






#endif /* cgo_wrapper_marshal_hpp */
