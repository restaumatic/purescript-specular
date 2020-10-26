## Next release

## 0.8.3

- 2020-10-26 Refactored `Slot` API to avoid using polytypes in records. This was done to enable experimenting with a certain modification of the PureScript compiler, which breaks in this case; and it causes no harm otherwise.

## 0.8.2

- 2020-08-19 Fixed a problem with event delivery order when using nested binds

## 0.8.1

- 2020-02-27 Specular.Dom.Element: Added functions to bind to input value and checkbox state
  - `valueD` - one-way binding to `value`
  - `bindValueOnChange`, `bindValueOnInput` - two-way binding to `value`
  - `checkedD` - one-way binding to `checked`
  - `bindChecked` - two-way binding to `checked`

## 0.8.0

- 2020-01-23 Expose Specular.Dom.Builder.getParentNode
- 2019-11-08 Added API guide in README
- 2019-11-06 Renamed some Ref functions for qualified importing
  - `refValue` -> `Ref.value`
  - `refUpdate` -> `Ref.modify`
  - `refUpdateConst` -> `Ref.set`

## 0.7.0

- 2019-10-30 Added a number of convenience functions:
   * `Specular.Callback`
     * `contramapCallbackDyn_`
   * `Specular.Dom.Element`
     *  `dynText`
     *  `attr`
     *  `attrWhen`
     *  `attrUnless`
     *  `attrWhenD`
     *  `attrUnlessD`
     *  `attrsD`
     *  `attrsWhen`
     *  `attrsUnless`
     *  `attrsWhenD`
     *  `attrsUnlessD`
     *  `classWhen`
     *  `classUnless`
   * `Specular.Dom.Widget`
     * `emptyWidget`
   * `Specular.FRP.Base`
     * `filterJustEvent`
     * `uniqDyn`
   * `Specular.FRP.Replacable`
     * `withDynamic_`
     * `whenJustD`
     * `whenD`
     * `unlessD`
- 2010-10-30 **(breaking change)** `Specular.Dom.Element.el_` accepts body, but does not accept props
- 2019-10-28 Added `Specular.Ref`
- 2019-10-24 Use a mutable queue instead of an immutable array ref

## 0.6.2

- 2019-10-23 Added Reader functionality to Widget

## 0.6.1

- 2019-10-23 Added missing exports to `Specular.Dom.Element`

## 0.6.0

- 2019-10-23 Monomorphized and extended `Specular.Dom.Element`
- 2019-10-10 **(breaking change)** Removed `Specular.FRP.for`. Use `<#>` from `Data.Functor` instead.

## v0.5.2

- 2019-09-25 `Dynamic` now has `Semigroup` and `Monoid` instances, lifting operations from the value type.

## v0.5.1

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
