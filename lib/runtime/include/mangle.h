#pragma once

#ifdef X86ISTMB_LINUX
    #define PREFIX() _x86istmb
#else
    #define PREFIX() x86istmb
#endif

#define __EVAL(x) x
#define _EVAL(x) __EVAL(__EVAL(x))
#define EVAL(x) _EVAL(_EVAL(x))

#define __PASTE(x, y) x##y
#define _PASTE(x, y) __PASTE(x, y)
#define PASTE(x, y) _PASTE(x, y)

#define X86ISTMB_SYM(s) PASTE(PREFIX(), PASTE(_S, s))
#define X86ISTMB_NAMESPACE_SYM(n, s)                                           \
    PASTE(PREFIX(), PASTE(_N, PASTE(n, PASTE(_S, s))))
