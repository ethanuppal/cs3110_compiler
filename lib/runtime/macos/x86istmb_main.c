#include <stdint.h>
#include "../include/x86istmb.h"

extern x86istmb_int X86ISTMB_SYM(main)(x86istmb_int, x86istmb_char**);

int main(int argc, char** argv) {
    return X86ISTMB_SYM(main)(argc, argv);
}
