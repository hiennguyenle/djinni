//
//  hello_impl.cpp
//  djinni_ios_example
//
//  Created by Mr Hien Nguyen Le Hien on 3/4/20.
//  Copyright © 2020 Fossil. All rights reserved.
//

#include "hello_impl.hpp"
#include "my_record.hpp"
#include <optional>

#include "json.hpp"
using json = nlohmann::json;

HelloImpl::HelloImpl() {
}

std::shared_ptr<textsort::Hello> textsort::Hello::create() {
    return std::make_shared<HelloImpl>();
}

textsort::my_enum HelloImpl::say_hi() {
    return textsort::my_enum::WORKOUT;
}

textsort::MyRecord HelloImpl::print(const textsort::MyRecord & rc) {
    
    std::string js_dump = ((json) rc).dump(2);
    
    printf(" : %s", js_dump.c_str());
    
    return rc;
}
