#include <unistd.h>
#include <sys/syscall.h>

#include "xgettid.h"

pid_t xgettid(void) {
        return syscall(SYS_gettid);
}
