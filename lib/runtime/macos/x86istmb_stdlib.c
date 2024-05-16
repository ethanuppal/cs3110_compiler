#include <stdio.h>
#include "../include/x86istmb.h"

void X86ISTMB_NAMESPACE_SYM(std, print_int)(x86istmb_int value) {
    printf("%lld", value);
}

void X86ISTMB_NAMESPACE_SYM(std, print_bool)(x86istmb_bool value) {
    printf("%s", value ? "true" : "false");
}

void X86ISTMB_NAMESPACE_SYM(std, print_char)(x86istmb_char value) {
    printf("%c", value);
}

void X86ISTMB_NAMESPACE_SYM(std, print_string)(x86istmb_char* str) {
    printf("%s\n", str);
}

void X86ISTMB_NAMESPACE_SYM(std, print_endline)(void) {
    printf("\n");
}
