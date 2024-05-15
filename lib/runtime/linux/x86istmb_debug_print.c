#include <stdio.h>
#include <stdint.h>
#define X86ISTMB_LINUX
#include "../include/mangle.h"

void X86ISTMB_NAMESPACE_SYM(std, debug_print)(int64_t value) {
    printf("%ld\n", value);
}
