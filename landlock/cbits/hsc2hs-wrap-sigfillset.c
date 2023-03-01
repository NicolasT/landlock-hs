#define _GNU_SOURCE
#include <signal.h>

extern int __real_sigfillset(sigset_t *set, int signum);

int __wrap_sigfillset(sigset_t *set, int signum) {
        return __real_sigfillset(set, signum);
}
