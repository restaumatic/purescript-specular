# Specular [![CircleCI](https://circleci.com/gh/restaumatic/purescript-specular/tree/master.svg?style=svg)](https://circleci.com/gh/restaumatic/purescript-specular/tree/master)

Specular is a library for building Web-based UIs in PureScript, based on
Functional Reactive Programming (FRP).

The API and DOM interaction is heavily inspired by [Reflex][reflex] and [Reflex-DOM][reflex-dom].
The FRP implementation is based on [Incremental](https://github.com/janestreet/incremental) (although the algorithm differs in some important ways).

## API

### FRP types

To use Specular effectively, you need to be familliar with some basic types.

#### [Dynamic](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.FRP.Base#t:Dynamic)

`Dynamic a` represents a read-only reference to a changing value of type `a`.

```purescript
-- | Read the current value of a `Dynamic`.
readDynamic :: forall m a. MonadEffect m => Dynamic a -> m a

-- | Execute the given action for the current value, and each new value when it changes.
subscribeDyn_ :: forall m a. MonadEffect m => MonadCleanup m => (a -> Effect Unit) -> Dynamic a -> m a
```

`Dynamic` is a `Monad`.

```purescript
-- `pure` creates a Dynamic that never changes.
pure "foo" :: Dynamic String

-- An applicative combination of Dynamics changes whenever one of them changes.
d1 :: Dynamic Int
d2 :: Dynamic Int
(+) <$> d1 <*> d2 :: Dynamic Int

-- Using the power of Monad we can choose which Dynamic to observe.
which :: Dynamic Bool
(which >>= if _ then d1 else d2) :: Dynamic Int
```

We can introduce new Dynamics using `newDynamic`, this is only required for top level components/widgets. Root dynamics
will be replaced by [Refs](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Ref#t:Ref) in the future, since they are almost the same.

```purescript
-- | Construct a new root Dynamic that can be changed from `Effect`-land.
newDynamic :: forall m a. MonadEffect m => a -> m { dynamic :: Dynamic a, read :: Effect a, set :: a -> Effect Unit, modify :: (a -> a) -> Effect Unit }
```

#### [Event](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.FRP.Base#t:Event)

`Event a` represents a source of occurences. Each occurence carries a value of type `a`.

`Event` is a `Functor`.

We can construct a trivial event `never :: forall a. Event a`, which never occurs.

Events can be combined:

```purescript
-- | An Event that occurs when any of the events occur. If some of them occur simultaneously, the occurence value is that of the leftmost one.
leftmost :: forall a. Array (Event a) -> Event a
```

Events can be transformed:

```purescript
-- | Retain only the occurences of the event for which the given predicate function returns `true`.
filterEvent :: forall a. (a -> Boolean) -> Event a -> Event a

-- | Map the given function over an Event, and retain only the occurences for which it returned a Just value.
filterMapEvent :: forall a b. (a -> Maybe b) -> Event a -> Event b

-- | Retain only the occurences of the Event which contain a Just value.
filterJustEvent :: forall a. Event (Maybe a) -> Event a
```

We can observe `Event`s by being notified of their occurences.

```purescript
-- | Execute the given action for each occurence of the Event.
subscribeEvent_ :: forall m a. MonadEffect m => MonadCleanup m => (a -> Effect Unit) -> Event a -> m a
```

#### [Callback](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Callback#t:Callback)

`Callback a` represents an action handler which consumes a value of type `a`. Think of it as `a -> Effect Unit`.
This represenation is likely to change in the future to `a -> Effect Unit`.

```purescript
-- | Trigger the action in Effect.
triggerCallback :: forall a. Callback a -> a -> Effect Unit
```

The DOM API that Specular exposes accepts callbacks, please also have a look at `Ref` bellow.

```purescript
-- | Create a button that triggers `removeTask` callback
el "button" [class_ "close", attrs ("type":="button" ), onClick_ $ removeTask task ] do
  text "Remove"

-- | Define a callback which modifies the content of `tasks` `Ref` by filtering out a task with a given id
let removeTask task = mkCallback \_ ->  triggerCallback (Ref.modify tasks) (filter $ \c -> c.id /= task.id)
```


#### [Ref](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Ref#t:Ref)

`Ref a` represents a read-write reference to a mutable observable variable.

We can think of a `Ref` as of `Effect.Ref`, but with additional functions:
- the ability to notify subscribers about changes to the value,
- the ability to focus using a lens.

`Ref a` consists of:
- `Ref.value :: Ref a -> Dynamic a` to observe the value
- `Ref.modify :: Ref a -> Callback (a -> a)` to modify the value using a function

As a shortcut we have `Ref.set :: Ref a -> Callback a` to replace the value completely.

Creating a Ref:

```purescript
newRef :: forall m a. MonadEffect m => a -> m (Ref a)
```

`Ref` is not a `Functor`, because it's read-write. It's `Invariant`, that is, it can be mapped over using a bijection.

This API will also likely change in the future, so that our interface resembles a [Ref](https://pursuit.purescript.org/packages/purescript-refs/5.0.0/docs/Effect.Ref#t:Ref)

### Building DOM content

#### [Widget](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Dom.Widget#t:Widget)

`Widget a` is a computation which can perform `Effects`, produce DOM nodes, subscribe to Events and Dynamics and returns a value of type `a`.

`Widget`s can be executed using `runMainWidgetInBody` - their contents will be inserted into the `document.body` element.

#### [Prop](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Dom.Element#t:Prop)

`Prop` is a modifier attached to a DOM element. Specific ways to construct a `Prop` are presented below.

#### [Attrs](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Dom.Browser#t:Attrs)

`Attrs` is a map of HTML attributes.

```purescript
-- A singleton map can be constructed using the `:=` operator.
"type":="button" :: Attrs

-- Attrs can be combined using the Monoid instance.
"type":="button" <> "name":="btn" :: Attrs
```

#### Static DOM

```purescript
import Specular.Dom.Element

-- | Produce a text node.
text :: String -> Widget Unit

-- | `el tag props body` - Produce a DOM Element.
-- |
-- | The elements produced by the `body` widget will be inserted as children of the element.
el :: forall a. TagName -> Array Prop -> Widget a -> Widget a

-- | `el tag props body` - Produce a DOM Element with no props.
el_ :: forall a. TagName -> Widget a -> Widget a

-- | Attach static attributes to the element.
attrs :: Attrs -> Prop

-- | Attach a static attribute to the element.
attrs :: AttrName -> AttrValue -> Prop

-- | Attach CSS classes to the element
classes :: [ClassName] -> Prop

-- | Attach a CSS class to the element
class_ :: ClassName -> Prop
```

For example, to produce the following HTML:

```html
<div class="alert alert-warning alert-dismissible fade show" role="alert">
  <strong>Holy guacamole!</strong> You should check in on some of those fields below.
  <button type="button" class="close" data-dismiss="alert" aria-label="Close">
    <span aria-hidden="true">&times;</span>
  </button>
</div>
```

One would write the following Specular code:

```purescript
el "div" [classes ["alert", "alert-warning", "alert-dismissible", "fade", "show"], attr "role" "alert"] do
  el_ "strong" $ text "Holy guacamole!"
  text " You should check in on some of those fields below."
  el "button" [class_ "close", attrs ("type":="button" <> "data-dismiss":="alert" <> "aria-label":="Close")] do
    el "span" [attr "aria-hidden"  "true"] do
      text "×"
```

#### Dynamic text, attributes and classes

Most of the `Prop` constructors have their dynamic counterparts. As a convention, their names end in `D`. For example:

```purescript
-- | Attach dynamic attributes to the element.
attrsD :: Dynamic Attrs -> Prop

-- | Attach dynamic CSS classes to the element
classesD :: Dynamic [ClassName] -> Prop
```

`text` also has a dynamic counterpart:

```purescript
-- | Create a text node whose text will reflect the value of the given Dynamic.
dynText :: Dynamic String -> Widget Unit
```

For convenience, utilities for common cases are provided such as:

```purescript
attrWhenD :: Dynamic Boolean -> AttrName -> AttrValue -> Prop
classWhenD :: Dynamic Boolean -> ClassName -> Prop
```

For example: assume you have `name :: Dynamic String`.
The code:

```purescript
let isLong nm = String.length nm >= 5
el "div" [class_ "name", classWhenD (isLong <$> name) "long"] do
  text "Your name is: "
  dynText name
```

when `name` has value `"Jan"`, would produce

```html
<div class="name">Your name is Jan</div>
```

whereas when `name` has value `"Titelitury"`, would produce

```html
<div class="name long">Your name is Titelitury</div>
```

#### Dynamic DOM structure

Sometimes changing text and attributes is not enough. For that there's `withDynamic_`:

```purescript
withDynamic_ :: forall a. Dynamic a -> (a -> Widget Unit) -> Widget Unit
```

Whenever the Dynamic changes, it will re-render a new `Widget` based on the latest value.

Example:

```purescript
-- Assume loading :: Dynamic Boolean

withDynamic_ loading $
  if _ then
    el "div" [class_ "loading"] $ text "Loading..."
  else
    el_ "div" do
      el_ "h1" $ text "Content"
      el_ "p" $ text "Bla bla bla"
```

Warning: Re-rendering a whole DOM block on each change has performance implications. Use with care.

#### Handling events

```purescript
-- | Connect a DOM event on the node to a Callback.
on :: EventType -> Callback DOM.Event -> Prop

-- | Shorthand: `on "click"`
onClick :: Callback DOM.Event -> Prop

-- | Like `onClick`, but takes a callback which ignores the DOM event.
onClick_ :: Callback Unit -> Prop
```

Example:

```purescript
-- Assume save :: Callback Unit

el "button" [attr "type" "button", onClick_ save] do
  text "Save"
```

For inputs we have predefined props that make `change` and `input` events handling easier (available in [Specular.Dom.Element](https://pursuit.purescript.org/packages/purescript-specular/docs/Specular.Dom.Element))

```purescript
-- * Input value
-- | Attach dynamically-changing `value` property to an input element.
-- | The value can still be changed by user interaction.
-- |
-- | Only works on `<input>` and `<select>` elements.
valueD :: Dynamic String -> Prop

-- | Set up a two-way binding between the `value` of an `<input>` element,
-- | and the given `Ref`.
-- |
-- | The `Ref` will be updated on `change` event, i.e. at the end of user inteaction, not on every keystroke.
-- |
-- | Only works on input elements.
bindValueOnChange :: Ref String -> Prop


-- | Attach dynamically-changing `checked` property to an input element.
-- | The value can still be changed by user interaction.
-- |
-- | Only works on input `type="checkbox"` and `type="radio"` elements.
checkedD :: Dynamic Boolean -> Prop

-- | Set up a two-way binding between the `checked` of an `<input>` element,
-- | and the given `Ref`.
-- |
-- | Only works on input `type="checkbox"` and `type="radio"` elements.
bindChecked :: Ref Boolean -> Prop

```

Example: 

```purescript

import Prelude
import Specular.Ref (Ref, newRef)
import Specular.Dom.Browser ((:=))
import Specular.Dom.Element (el, attrs, bindValueOnChange)
import Specular.Dom.Widget (emptyWidget)

let description :: Ref String = newRef ""

el "input" [attrs ("type" := "text")    , bindValueOnChange description] emptyWidget

```


#### A Counter example

```purescript
module Counter where

import Prelude
import Effect (Effect)

import Data.Functor.Contravariant (cmap)

import Specular.Callback (mkCallback, triggerCallback)
import Specular.Dom.Browser ((:=))
import Specular.Dom.Element (attrs, class_,  el,  onClick_, text, dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Ref (Ref, value, newRef, modify)



counterWidget :: Effect Unit
counterWidget = do
  -- | Will append widget to the body
  runMainWidgetInBody do
    counter :: Ref Int <- newRef 0
  
    -- | Subtract 1 from counter value the straight forward way
    let subtractCb = mkCallback \_ -> triggerCallback (modify counter) (add 1)

    -- | Add 1 to counter value using the contravariant instance
    let addCb = cmap (\_ -> add 1) (modify counter)

    el "button" [class_ "btn", attrs ("type":="button" ), onClick_ addCb ] do
      text "+"

    dynText $ show <$> value counter

    el "button" [class_ "btn", attrs ("type":="button" ), onClick_ subtractCb ] do
      text "-"
```



## Why not just use Reflex and GHCJS?

In short: code size. Specular demos are 240K unminified (with DCE - `pulp build
-O`), or 19K minified with `uglifyjs -c -m` and gzipped. In contrast, a a GHCJS
(`0.2.1.9007019`) program that prints `Hello World` (no DOM bindings included,
just `base`) weighs `1.1M` unminified, or 62K minified with Closure Compiler's
`ADVANCED_OPTIMIZATIONS` and gzipped. Supporting Haskell semantics has a cost.

There are also other reasons, of course.

## Why not use other PureScript UI libraries?

See [Motivation](doc/Motivation.md).

## Limitations

Some of the cons of Specular:

- No good way to do server-side rendering. Local state complicates this.

- Performance may be sometimes bad, because it does not use any Virtual DOM -
  the element placement instructions you write translate pretty much directly to
  `createElement`/`appendChild`. There are no (representative) benchmarks yet.

- Time travel debugging, as known from Elm, is not possible.

- Currently no way to bind to React Native.

- Programs written with Specular may be harder to understand for some people who
  prefer the single state variable approach.

- Compared to Reflex, it has way less FRP combinators.

- Creating recursive data flows is more cumbersome than in Reflex, because
  PureScript has eager evaluation and no `RecursiveDo`.

- It's immature and not popular, and may have bugs.

If you think there are more, please open an issue. They should be listed.

[reflex]: https://github.com/reflex-frp/reflex
[reflex-dom]: https://github.com/reflex-frp/reflex-dom

## Who's using it?

- **Restaumatic** - used in production for a signification portion of online ordering frontend, as well as for backoffice apps and our mobile app for restaurants.

## Contact

If you discover bugs, want new features, or have questions, please post an issue using the GitHub issue tracker.

You can also contact `@mbieleck` on [FP Chat](https://fpchat-invite.herokuapp.com/), if you want to chat about Specular.
