#include <stdio.h>
#include <stdint.h>
#include "../include/mangle.h"

void X86ISTMB_NAMESPACE_SYM(std, debug_print)(int64_t value) {
    printf("%lld\n", value);
}
