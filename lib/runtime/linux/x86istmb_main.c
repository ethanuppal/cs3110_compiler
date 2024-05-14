#include <stdint.h>
#define X86ISTMB_LINUX
#include "../include/mangle.h"

extern int64_t X86ISTMB_SYM(main)(void);

int main() {
    return X86ISTMB_SYM(main)();
}
