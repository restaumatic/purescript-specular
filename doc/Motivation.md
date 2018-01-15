## Why use Specular instead of other PureScript UI libraries?

### Survey of the available libraries

#### [Elm][elm] / [Pux][pux] / [Miso][miso]

And countless other implementations of
[The Elm Architecture](https://guide.elm-lang.org/architecture/).

There is only one state variable, storing an immutable data structure.

An application consists of:

- `type Model` - the data that is stored in the state variable, typically a
     product type

- `type Msg` - a data type representing possible application events, typically
     a sum type

- `render :: Model -> Html Msg`\[1\]

- `update :: Msg -> Model -> (Model, Cmd Msg)`\[2\]

- `init :: Model`

\[1\]: where `Html a` is some representation of the DOM which produces events of type `a`

\[2\]: where `Cmd` is an asynchronous effect monad

"Subcomponents" have the same "structure" (Model + Msg + render + update + init).

"Subcomponents" are embedded by:

- Adding a field in `Model` for the subcomponent `Model`
- Adding a constructor in `Msg` that wraps `Msg`s of the subcomponent
- In `render`, passing the relevant part of state to child `render`
- In `update`, handling the wrapping `Msg` by selecting the relevant part of
  state, invoking child `update`, modifying the relevant part of state,
  massaging `Cmd`s so that they return the wrapped `Msg`s instead of child
  `Msg`s.

There's also an alternative approach to `Msg` wrapping: make the child component
polymorphic in its returned `Msg` type, and pass an injection function
`ChildMsg -> msg`.

[elm]: http://elm-lang.org/
[pux]: http://purescript-pux.org/
[miso]: https://haskell-miso.org/

#### [Halogen](https://github.com/slamdata/purescript-halogen)

Each Component has its local state.

A Component consists of:

- `type State` - similar to `type Model` in Elm

- `type Query a` - similar to `type Msg` in Elm, but instead of just an
    enumeration of possible actions, this describes possible _interactions_ with
    the component - they may return a value.

- `type Message` - a sum type of events that the component may sent to the
  parent.

- `render :: State -> H.ComponentHTML Query` - similar to Elm's `render`

- `eval :: Query ~> H.ComponentDSL State Query Message m` - a function that
  handles a Query in a monad. Available actions in this monad are: change the
  state, issue a query to child components, send a `Message` to the parent, or
  do IO (`Aff`).

- `initialState :: State`

See [Defining a component](https://github.com/slamdata/purescript-halogen/blob/master/docs/2%20-%20Defining%20a%20component.md) for more details.

There is support for embedding subcomponents. You have to define:

- `type Slot` - type representing an identifier of a child component

- a constructor in `Query` for handling messages from each child component (if
  it sends any Messages)

See [Parent and child components](https://github.com/slamdata/purescript-halogen/blob/master/docs/5%20-%20Parent%20and%20child%20components.md) for more details.

#### [Thermite](https://github.com/paf31/purescript-thermite)

There is only one state variable. So in this regard it is fundamentally similar
to Elm.

To define a component, you provide:

- `type State`

- `type Action`

- `initialState :: State`

- `render :: T.Render State Action` - similar to Elm's render

- `performAction :: T.PerformAction _ State _ Action` - something in between
  Elm's `update` and Halogen's `eval`. Handles an `Action` in a monad. The monad
  may do asynchronous IO and modify state as many times as you like. The
  difference from Halogen is that there's no support for querying child
  components.

There are also some lens-based combinators that simplify the wrapping and
unwrapping inherent to Elm.

#### [Concur](https://github.com/ajnsit/concur)

> A brand new client side Web UI framework for Haskell that explores an entirely
> new paradigm. It does not follow FRP (think Reflex or Reactive Banana), or Elm
> architecture, but aims to combine the best parts of both.

_Note: the programming model is very different from the usual ones, and I may be
misunderstanding it._

Widgets are build in a monad. Each computation produces a chunk of HTML and
pauses, waiting for an interaction. `<|>` runs two computations concurrently and
concatenates their HTML chunks.

Internal state is done by using `StateT` inside the branches.

Input to widgets is passed via function arguments.

Output is returned by the monadic computation when it finishes.

This means that on each interaction with the outside, a widget's internal state
is destroyed.

This makes the local state feature less usable, because you often need to avoid
it when there are complex interactions with the outside. I'll exclude Concur
from the examples.

#### [Flare](https://github.com/sharkdp/purescript-flare)

> Flare is a special-purpose UI library for PureScript. It is built on top of
> purescript-signal and uses Applicative-style programming to combine predefined
> input fields to a reactive user interface.

It has very little support for dynamically changing the UI structure, so it's
not suitable as a general purpose library. I'll exclude Flare from the examples.

#### [Reflex](https://github.com/reflex-frp/reflex) / Specular

Widgets are build in a monad. Each computation produces a chunk of concrete HTML
elements and returns immediately. `>>=` concatenates HTML chunks.

Input to components is provided by passing FRP primitives (`Event`s and
`Dynamic`s) as arguments to the construction function. The widget will
automatically reflect changes in them.

Output is done by returning `Event`s and `Dynamic`s from the widget construction
function.

In contrast to Concur, the internal state lifecycle is not tied to input and
output.

Some more material:

- [A Beginner-friendly Step by Step Tutorial for Reflex-Dom](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)

I believe that this approach gives the most flexibility:

- You can emulate Elm by having a single `Dynamic` storing the application
  state, and collecting all `Events` into a single giant `Event` that modifies
  it.

- You can't exactly emulate Halogen, but instead of imperatively querying a
  component's internal state, you can just have a Dynamic that represents it.

- You can implement a Flare-like thing very easily
    (`newtype Flare a = Flare (Compose Widget Dynamic a)` + derived Applicative
    instance)

- You can use FRP to express things like event throttling, which is necessary
  when querying the server based on some rapidly changing input, and very
  tedious to implement in Elm-like architectures.

### Examples

Here I'll show some example development tasks, and compare some of the libraries
using them.

#### Routing

You have an application with a lot of subpages ("routes"). You probably
represent the routes as a sum type (`Route`). Based on the current route, you want to
render different pages. Each page mayhave to fetch some data when loading.

##### Elm

See [elm-spa-example](https://github.com/rtfeldman/elm-spa-example/blob/master/src/Main.elm).

Because there's only global state, you have no choice but to have:

- another sum type mirroring the `Route` type, but storing also the `Model` of each page

- a data constructor in the top-level `Msg` wrapping internal `Msg`s of each
  page

- [boilerplate code](https://github.com/rtfeldman/elm-spa-example/blob/28d9288c7a67cf53bb628acfba79689bc5516509/src/Main.elm#L400)
  in the update function wrangling the Model and Msg wrapping

- More boilerplate handling initialization of each page during route change

##### Halogen

You can define a component per page. In the main component, you can reuse the
`Route` as `Slot`. Components have local state, so you don't have to know about
what's happening in the subpages at the top level.

As far as I know, there's no such thing as "initial effect" - you have to send
the pages some kind of `Init` message on route change to let them fetch their
data.

##### Specular

You can have a `currentRoute :: Dynamic Route` and a function `routeWidget ::
Route -> Widget Unit`. Then the main widget is:

```
dynamic_ $ map routeWidget currentRoute
```

The page state will be reset on each change of `currentRoute`.

#### Instant Search

User types some text in a text field, and the application should fetch data
based on the input and display it to the user.

The requests may take a while, so we will display `Loading...` while it's
loading.

Note that the responses may arrive in a different order than the requests. We
want to maintain the property that if the user stops typing, eventually the
displayed response will match the input text (responses to earlier requests
should not override later ones).

We'll ignore errors for the purpose of this example; assume they are handled the
same way as success responses (i.e. displayed).

##### Elm

Naively:

```
data Msg =
    SearchStarted String
  | SearchFinished Response

type alias Model =
  { query : String
  , response : RemoteData Error Response
  }

render : Model -> Html Msg
render model =
  div []
    [ input [ type_ "text", onInput SearchStarted ] []
    , case model.response of

        Loading ->
          text "Loading..."

        Success x ->
          displayResponse x

        _ -> text ""
    ]

update :: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    SearchStarted query ->
      ( { model | query = query, response = Loading }
      , Backend.query query SearchFinished
      )

    SearchFinished response ->
      ( { model | response = Success response }
      , noCmd
      )
      
```

The code looks relatively straightforward, but suffers from the flaw mentioned
in the introduction: responses arriving out of order.

To fix this, we'll need to thread the query value with the response:

```
data Msg =
    SearchStarted String
  | SearchFinished String Response -- query, response
```

and when it arrives, we'll need to check if it's for the current query:

```
    SearchFinished query response ->
      ( if query == model.query
          then { model | response = Success response }
          else model
      , noCmd
      )
```

Ideally we'd like to cancel the currently running request as soon as we know
that we'll ignore the response - that is, when the query value changes. But that
would complicate things.

##### Halogen

In Halogen the solution would be similar to Elm, with one important difference:
we don't need the `ResponseFinished` action. The action handling functions in
Halogen can be asynchronous. So the `eval` would look like:

```
eval = case _ of
  SearchStarted query next ->
    H.modify (_ { query = query, response = Loading })
    response <- liftAff $ Backend.query query
    currentQuery <- H.asks _.query
    when (query == currentQuery) $
      H.modify { response = Success response }
```

Note that after `Backend.query` we still need to check whether another request
wasn't started in the meantime. That's because there can be several action
handlers running concurrently.

##### Specular

Assuming we have a Dynamic representing the input:

```
(query :: Dynamic String) <- textInputOnInput "" mempty
```

and a function to perform the query (again, ignoring errors):

```
Backend.query :: String -> IO Response
```

we can use the `asyncRequest` combinator to perform a request every time the
query changes. The previous request will be automatically cancelled (using Aff's
cancellation semantics) so that the responses won't arrive out of order (in
fact the cancelled response shouldn't arrive at all).

```
(response :: Dynamic (RequestState Response)) <- asyncRequest $ map Backend.query query
```

The response can be then displayed:

```
dynamic_ $ for response $ \response' ->
  case response' of
    Loading ->
      text "Loading..."

    Loaded x ->
      displayResponse x

    _ -> text ""
```

The implementation of `asyncRequest` can be more complicated than the previous
cases, but it can be done once and reused quite easily.

#### Instant Search with throttling

NOTE: This is currently an exercise for the reader.

Consider how would you add throttling to the previous example. Requirements:

1. The application will send no more that one request per 300 milliseconds.
2. After input changes stop, eventually the displayed response will reflect the
   current value.
