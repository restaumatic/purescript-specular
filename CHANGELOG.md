## Pre-release

- 2018-05-07 **(breaking change)** The `MonadPull`, `MonadHost`,
  `MonadHostCreate` and `MonadHold` type classes are removed in interest of
  simplicity. The remaining ones are:

  - `MonadIOSync` - almost all operations require `IOSync`
  - `MonadCleanup` - operations that subscribe to events require `onCleanup` to
    register the unsubscribe handler

  - `MonadFRP` - a convenient alias for `(MonadIOSync m, MonadCleanup m)`.

  - `MonadReplace` - for dynamic DOM fragments
  - `MonadDetach` - for dynamic DOM fragments

- 2018-05-02 **(breaking change)** The `MonadHostCreate` and `MonadHost` classes
  became single-parameter - the `io` parameter was removed. It was intended to
  abstract over `IOSync` and potentially run the whole system in a "pure" way,
  but the idea didn't turn out well.
