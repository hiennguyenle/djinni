//
// Copyright 2014 Dropbox, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#pragma once

#include "Primitive.hpp"

namespace djinni {

class HI16 : public Primitive<HI16, int16_t, jshort> {
    HI16() : Primitive("java/lang/Short",
                       "valueOf", "(S)Ljava/lang/Short;",
                       "shortValue", "()S") {}
    friend class JniClass<HI16>;
};

} // namespace djinni
