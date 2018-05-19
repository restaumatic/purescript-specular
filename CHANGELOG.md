## Pre-release

- 2018-05-20 The `MonadPull`, `MonadHost`, `MonadHostCreate` and `MonadHold`
  type classes are reduced to just "aliases" for `MonadCleanup`+`MonadIOSync`
  in interest of simplicity.

  It is advised to replace uses of the above classes with `MonadFRP`. They will
  be removed in a future version.

- 2018-05-02 **(breaking change)** The `MonadHostCreate` and `MonadHost` classes
  became single-parameter - the `io` parameter was removed. It was intended to
  abstract over `IOSync` and potentially run the whole system in a "pure" way,
  but the idea didn't turn out well.

  To fix breakage, remove parameter from your uses of `MonadHost` and
  `MonadHostCreate`.
