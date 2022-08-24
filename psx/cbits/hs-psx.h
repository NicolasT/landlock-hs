#ifndef _HS_PSX_H_
#define _HS_PSX_H_

#include <signal.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Ensure this is in sync with whatever libpsx uses */
#define HS_PSX_SIGNAL SIGSYS

long hs_psx_syscall3(long num, long arg1, long arg2, long arg3);
long hs_psx_syscall6(long num, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6);

#ifdef __cplusplus
}
#endif

#endif
