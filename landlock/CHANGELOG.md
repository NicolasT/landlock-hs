# Revision history for landlock

## 0.2.2.0 -- YYYY-MM-DD

* Update Cabal file formatting.

* Support GHC 9.6.4 / `base ^>=4.18`.

* Support GHC 9.8.2 / `base ^>=4.19`.

* Support `optparse-applicative ^>=0.18`.

* Support `tasty ^>=1.5`.

* Support `filepath ^>=1.5`.

## 0.2.1.1 -- 2023-02-28

* Extend API documentation with links to man-pages.

## 0.2.1.0 -- 2023-02-22

* Use vendored `linux/landlock.h` instead of system-provided header during
  build.

* Support Landlock ABI v2 and `LANDLOCK_ACCESS_FS_REFER` as part of it.

* Support Landlock ABI v3 and `LANDLOCK_ACCESS_FS_TRUNCATE` as part of it.

* Support GHC 9.4.2 / `base ^>=4.17`.

* Support `unix ^>=2.8`.

* Add a new executable,`landlocked`, which permits to run a command in
  a sandboxed environment.

* Add a flag, `landlocked`, which allows to not build the `landlocked`
  executable.

* Use `capi` foreign imports instead of `ccall` using the `CApiFFI` language
  extension.

* Add a Cabal flag, `werror`, to enable compiler `-Werror` and friends.

## 0.2.0.1 -- 2022-08-24

* Code-wise the same as version 0.2.0.0, but said version was incorrectly
  published on Hackage, hence re-releasing.

## 0.2.0.0 -- 2022-08-24

* Support `base ^>=4.15` and `base ^>=4.16`, tested in CI using GHC 9.0.2 and
  GHC 9.2.2.

* Add a dependency on `psx` to avoid `setxid`-style security issues.

  See `test/ThreadedScenario.hs` for a scenario which triggers the
  aforementioned security issue, which needs a Glibc `setxid`-style
  work-around. This scenario is executed by the new `landlock-test-threaded`
  test-suite.

  See https://github.com/NicolasT/landlock-hs/issues/9 for more background.

## 0.1.0.0 -- 2022-08-18

* First version. Released on an unsuspecting world.
