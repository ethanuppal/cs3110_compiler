#include <stdio.h>
#include "../include/x86istmb.h"

void X86ISTMB_NAMESPACE_SYM(std, debug_print_int)(x86istmb_int value) {
#ifdef X86ISTMB_LINUX
    printf("%ld\n", value);
#else
    printf("%lld\n", value);
#endif
}