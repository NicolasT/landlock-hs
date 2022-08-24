#include <stdlib.h>

long __attribute__((weak)) hs_psx_syscall3();
long __attribute__((weak)) hs_psx_syscall6();

int detect_psx(void) {
        return &hs_psx_syscall3 == NULL || &hs_psx_syscall6 == NULL ? 0 : 1;
}
