#ifndef _HS_LANDLOCK_H_
#define _HS_LANDLOCK_H_

#include <stddef.h>
#include <linux/landlock.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

long landlock_create_ruleset(const struct landlock_ruleset_attr *const attr,
                             const size_t size,
                             const __u32 flags);
long landlock_add_rule(const int ruleset_fd,
                       const enum landlock_rule_type rule_type,
                       const void *const rule_attr,
                       const __u32 flags);
long landlock_restrict_self(const int ruleset_fd,
                            const __u32 flags);

#ifdef __cplusplus
}
#endif

#endif
