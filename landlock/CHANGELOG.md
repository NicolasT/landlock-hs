# Revision history for landlock

## 0.2.0.0 -- YYYY-mm-dd

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