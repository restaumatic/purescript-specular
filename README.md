# Specular

Specular is a library for building Web-based UIs in PureScript, based on
Functional Reactive Programming (FRP). It is heavily inspired by
[Reflex][reflex] and [Reflex-DOM][reflex-dom] and supports a similar programming
model.

## Getting started

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

In short: code size. Specular demos are 212K unminified (with DCE - `pulp build
-O`), or 23K minified with uglifyjs and gzipped. In contrast, a a GHCJS
(`0.2.1.9007019`) program that prints `Hello World` (no DOM bindings included,
just `base`) weighs `1.1M` unminified, or 62K minified with Closure Compiler's
`ADVANCED_OPTIMIZATIONS` and gzipped. Supporting Haskell semantics has a cost.

There are also other reasons, of course.

## Why not use other PureScript UI libraries?

See [Motivation](doc/Motivation.md).

[reflex]: https://github.com/reflex-frp/reflex
[reflex-dom]: https://github.com/reflex-frp/reflex-dom
