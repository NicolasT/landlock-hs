#include <stddef.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <hs-psx.h>

#include "hs-landlock.h"
#include "linux/landlock.h"

long hs_landlock_create_ruleset(const struct landlock_ruleset_attr *const attr,
                                const size_t size,
                                const __u32 flags) {
#ifdef landlock_create_ruleset
        return landlock_create_ruleset(attr, size, flags)
#else
        return syscall(__NR_landlock_create_ruleset, attr, size, flags);
#endif
}

long hs_landlock_add_rule(const int ruleset_fd,
                          const enum landlock_rule_type rule_type,
                          const void *const rule_attr,
                          const __u32 flags) {
#ifdef landlock_add_rule
        return landlock_add_rule(ruleset_fd, rule_type, rule_attr, flags)
#else
        return syscall(__NR_landlock_add_rule, ruleset_fd, rule_type, rule_attr, flags);
#endif
}

long hs_landlock_restrict_self(const int ruleset_fd,
                               const __u32 flags) {
        return hs_psx_syscall3(__NR_landlock_restrict_self, ruleset_fd, flags, 0);
}

int hs_landlock_prctl(int option,
                      unsigned long arg2,
                      unsigned long arg3,
                      unsigned long arg4,
                      unsigned long arg5) {
        return hs_psx_syscall6(__NR_prctl, option, arg2, arg3, arg4, arg5, 0);
}
