# Specular [![CircleCI](https://circleci.com/gh/restaumatic/purescript-specular/tree/master.svg?style=svg)](https://circleci.com/gh/restaumatic/purescript-specular/tree/master)

Specular is a library for building Web-based UIs in PureScript, based on
Functional Reactive Programming (FRP). It is heavily inspired by
[Reflex][reflex] and [Reflex-DOM][reflex-dom] and supports a similar programming
model.

## Getting started

There's a tutorial currently in the making: [Tutorial (incomplete)](./doc/Tutorial.md)

Currently there's no tutorial or introductory documentation. However, there's
plenty of material about Reflex-Dom. See
[A Beginner-friendly Step by Step Tutorial for Reflex-Dom](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)
for a good introduction to the programming model. It should be adapted to
PureScript and Specular in the near future.

There are some [examples](test/browser/examples) that are also integration
tests. They can be viewed in a Web browser:

```
npm run build-demo
$WWW_BROWSER test/demo.html
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
  `createElement`/`appendChild`. There are no benchmarks yet.

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
