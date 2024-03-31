/* Stubs for linking `hsc2hs` programs to succeed.
 *
 * For some reason, `cabal` passes a library's `ld-options` to the C compiler
 * when building `hsc2hs` C files. However, in our case, these flags request
 * wrapping of (e.g.) `sigfillset`. On some platforms, `libgcc` gets linked in
 * as well, and on some platforms this `libgcc` relies on `sigfillset`, hence
 * linking fails unless `__wrap_sigfillset` is declared.
 *
 * This module relays calls to the underlying `__real_*` implementation.
 *
 * See https://github.com/haskell/cabal/issues/8824
 */

#include <signal.h>

extern int __real_sigfillset(sigset_t *set);

int __wrap_sigfillset(sigset_t *set) {
        return __real_sigfillset(set);
}
