# Revision history for psx

## 0.1.2.0 -- YYYY-MM-DD

* Update Cabal file formatting
* Support GHC 9.6.4 /  `base ^>=4.18`

## 0.1.1.1 -- 2023-02-28

* Remove `-Wl,-undefined,__wrap_sigfillset` from link options.

## 0.1.1.0 -- 2023-02-22

* Support GHC 9.4.2 / `base ^>=4.17`.

* Rely on `_POSIX_C_SOURCE >= 1` instead of `_GNU_SOURCE` as feature test macro
  for `sigset_t` in `hs-psx.c`.

* Use `capi` foreign imports instead of `ccall` using the `CApiFFI` language
  extension.

* Include a `Setup.hs` file.

* Minor stylistic changes to Cabal package description file.

* Add a Cabal flag, `werror`, to enable compiler `-Werror` and friends.

## 0.1.0.0 -- 2022-08-24

* First version. Released on an unsuspecting world.
