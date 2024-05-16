#include <stdio.h>
#include "../include/x86istmb.h"

void X86ISTMB_NAMESPACE_SYM(std, debug_print_int)(x86istmb_int value) {
    printf("%lld\n", value);
}
