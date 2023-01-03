#ifndef _HS_LANDLOCK_H_
#define _HS_LANDLOCK_H_

#include <stddef.h>
#include <sys/types.h>

#include "linux/landlock.h"

#ifdef __cplusplus
extern "C" {
#endif

long hs_landlock_create_ruleset(const struct landlock_ruleset_attr *const attr,
                                const size_t size,
                                const __u32 flags);
long hs_landlock_add_rule(const int ruleset_fd,
                          const enum landlock_rule_type rule_type,
                          const void *const rule_attr,
                          const __u32 flags);
long hs_landlock_restrict_self(const int ruleset_fd,
                               const __u32 flags);

int hs_landlock_prctl(int option,
                      unsigned long arg2,
                      unsigned long arg3,
                      unsigned long arg4,
                      unsigned long arg5);

#ifdef __cplusplus
}
#endif

#endif
