## Pre-release

- 2018-05-07 **(breaking change)** The `MonadPull`, `MonadHost`,
  `MonadHostCreate` and `MonadHold` type classes are removed in interest of
  simplicity.

  To fix breakage, replace uses of the above classes with `MonadFRP`.

- 2018-05-02 **(breaking change)** The `MonadHostCreate` and `MonadHost` classes
  became single-parameter - the `io` parameter was removed. It was intended to
  abstract over `IOSync` and potentially run the whole system in a "pure" way,
  but the idea didn't turn out well.

  To fix breakage, remove parameter from your uses of `MonadHost` and
  `MonadHostCreate`.
