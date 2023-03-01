#include <signal.h>

int __real_sigfillset(sigset_t *set, int signum);

int __wrap_sigfillset(sigset_t *set, int signum) {
        return __real_sigfillset(set, signum);
}
