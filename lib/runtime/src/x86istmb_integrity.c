#include <stdio.h>
#include <stdlib.h>
#include "../include/x86istmb.h"

int X86ISTMB_NAMESPACE_SYM(runtime, puts)(const char *s) {
    return puts(s);
}

void X86ISTMB_NAMESPACE_SYM(runtime, exit)(int code) {
    exit(code);
}
