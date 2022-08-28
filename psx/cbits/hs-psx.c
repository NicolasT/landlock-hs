#define _POSIX_C_SOURCE 1

#include <signal.h>

#ifdef BUNDLED_LIBPSX
# include "psx/psx_syscall.h"
#else
# include <sys/psx_syscall.h>
#endif

#include "hs-psx.h"

/* This will be provided by the dynamic linker when linked with -Wl,-wrap,sigfillset */
extern int __real_sigfillset(sigset_t *set);

int __wrap_sigfillset(sigset_t *set) {
        int rc;

        rc = __real_sigfillset(set);
        if(rc != 0) {
                return rc;
        }

        return sigdelset(set, HS_PSX_SIGNAL);
}

long hs_psx_syscall3(long num, long arg1, long arg2, long arg3) {
        return psx_syscall3(num, arg1, arg2, arg3);
}

long hs_psx_syscall6(long num, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6) {
        return psx_syscall6(num, arg1, arg2, arg3, arg4, arg5, arg6);
}
