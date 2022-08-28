#include <stddef.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <hs-psx.h>

#include "hs-landlock.h"
#include "linux/landlock.h"

#ifndef landlock_create_ruleset
long landlock_create_ruleset(const struct landlock_ruleset_attr *const attr,
                             const size_t size,
                             const __u32 flags) {
        return syscall(__NR_landlock_create_ruleset, attr, size, flags);
}
#endif

#ifndef landlock_add_rule
long landlock_add_rule(const int ruleset_fd,
                       const enum landlock_rule_type rule_type,
                       const void *const rule_attr,
                       const __u32 flags) {
        return syscall(__NR_landlock_add_rule, ruleset_fd, rule_type, rule_attr, flags);
}
#endif

#ifndef landlock_restrict_self
long landlock_restrict_self(const int ruleset_fd,
                            const __u32 flags) {
        return hs_psx_syscall3(__NR_landlock_restrict_self, ruleset_fd, flags, 0);
}
#endif

int hs_landlock_prctl(int option,
                      unsigned long arg2,
                      unsigned long arg3,
                      unsigned long arg4,
                      unsigned long arg5) {
        return hs_psx_syscall6(__NR_prctl, option, arg2, arg3, arg4, arg5, 0);
}
