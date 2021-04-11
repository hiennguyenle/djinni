//
//  cgo_wrapper_marshal.h
//  demo
//
//  Created by Hien Nguyen on 13/01/2021.
//

#ifndef cgo_wrapper_marshal_h
#define cgo_wrapper_marshal_h
#include <stdint.h>
#include <stdio.h>

// Cgo string
typedef struct cgo__string {
    size_t length;
    const char * str;
} cgo__string;

void free_cgo_string(cgo__string * ptr);

// Cgo Binary
typedef struct {
    size_t length;
    const uint8_t * data;
} cgo__binary;

void free_cgo_binary(cgo__binary * ptr);


#endif /* cgo_wrapper_marshal_h */
