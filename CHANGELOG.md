## Next release

- 2019-08-08 Fixed a bug where `asyncRequestMaybe` would return an incorrect
  result when passed a synchronous initial action.

## v0.5.0

- 2019-08-07 `asyncRequest` now delivers the change to `Loaded` synchronously if
  the action is synchronous. It was asynchronous before as a workaround for
  events not being delivered in the correct order.
- 2019-08-07 Event subscribers now receive values in order they were fired, even
  when an event is fired from inside a subscriber (issue #43). Fix by @przembot.

## v0.4.1

- 2019-06-18 Fixed a bug in `textInputValueOnChange`. A value changed by
  `setValue` event was lost if the user subsequently edited text field. By
  @przembot

## v0.4.0

- 2019-06-01 **(breaking change)** PureScript updated to `0.13.0`.

## v0.3.0

- 2018-09-14 `RequestState` now has a `Functor` instance.
- 2018-09-04 Added lifted Semigroup and Monoid instances for Builder, and a constraint `Monoid (m Unit)` to MonadWidget.
- 2019-03-18  **(breaking change)** The `DOM` and `EventDOM` type classes are
  removed. Member functions are now specialized to `Specular.Dom.Browser.Node`,
  and are moved to this module. The `node` parameter from `MonadDomBuilder` was
  removed.
- 2019-03-19 Added new experimental DOM builder API - `Specular.Dom.Element`.
- 2019-03-30 Fixed a bug where a `checkboxView` widget would stop being updated
  from the JavaScript side as soon as it was touched by the user. By @kfigiela.

## v0.2.0

- 2018-08-30 Added HeytingAlgebra instance for Dynamic that just lifts the operations via Applicative.

- 2018-08-08 `Debug.Specular` renamed to `Specular.Debug` (issue #30).

- 2018-08-08 `asyncRequest` and `asyncRequestMaybe` no longer require `MonadReplace` constraint.

- 2018-07-31 **(breaking change)** Updated to PureScript 0.12. This induced some breaking API changes:
  
   - Effectful functions now use `Effect` instead of `IOSync`
   - `Attrs` is now `Foreign.Object`, not `StrMap`

## Pre-release

- 2018-06-04 `dynamicList`, `dynamicListWithIndex`, `dynamicList_` and
  `dynamicListWithIndex_` functions were added.

- 2018-06-03 `uniqWeakDynBy` function was added, analogous to `uniqDynBy`.

- 2018-05-31 **(breaking change)** The `hostEffect` function was removed.
  Replace all uses with `liftIOSync`.

- 2018-05-31 The modules `Data.DelayedEffects`, `Data.IORef`, and
  `Control.Monad.RIO` are moved to an internal namespace. The change is breaking
  only if you were using them.

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
