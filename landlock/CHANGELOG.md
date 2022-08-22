# Revision history for landlock

## 0.2.0.0 -- YYYY-mm-dd

* Support `base ^>=4.15` and `base ^>=4.16`, tested in CI using GHC 9.0.2 and
  GHC 9.2.2.

* Due to potential security issues when using `landlock` in a threaded
  environment, the function now throws an exception when the threaded RTS is
  used. The new `unsafeLandlock` function doesn't, but should likely not be
  used.

  This is an API change, both in the type of the `landlock` function, which
  now requires a `MonadFail` constraint, as well as in its runtime behaviour.

  See `test/ThreadedScenario.hs` for a scenario which triggers the
  aforementioned security issue, which would need a Glibc `setxid`-style
  work-around. This scenario is executed by the new `landlock-test-threaded`
  test-suite.

  See https://github.com/NicolasT/landlock-hs/issues/9 for mor background.

## 0.1.0.0 -- 2022-08-18

* First version. Released on an unsuspecting world.
