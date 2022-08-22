# psx: Haskell bindings for libpsx

This library embeds libpsx in a GHC Haskell-compiled application.

Note `libpsx` performs some trickery with signal handling in a process.
Furthermore, when using this library, `sigfillset` will be wrapped so
`SIGSYS` is *not* set, in order for the GHC RTS `ticker` thread not to
block the signal and work properly with `libpsx`.

See
[this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/533)
for a potential future replacement of this library using functionality built
into the GHC RTS.
