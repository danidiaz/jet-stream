# jet-stream

This is yet another streaming library for Haskell, created to scratch the
following itches:

- The main type is as simple as possible: the only type parameter is the type
  of the yielded elements.

- The `Monoid` / `Alternative` / `MonadPlus` methods perform concatenation,
  just like with regular lists. The `Functor` `Applicative` and `Monad`
  instances also resemble those of lists.

- There are direct analogues of functions like `withFile`, `bracket`, `finally`
  and `onError` that easy to integrate in a streaming pipeline, and behave
  smartly when combined with functions like `take`.

- Compatible with the [foldl](https://hackage.haskell.org/package/foldl)
  library for collector-like terminal operations. (All self-respecting
  streaming libraries must have this.)

In order to achieve those objectives, the following sacrifices have been made:

- No flexibility in the underlying monad for the stream effects: it's always
  `IO`.

- No separate "channels" that return extra information at the end of the
  stream. This means exceptions are the only way of signalling errors or
  unexpected conditions.

- Elements in a stream can't be "extracted" one by one in a pull-based way,
  like you can do for example in
  [streaming](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:next).

- There's `take` and `drop`, but not at proper `splitAt`. Also, grouping
  operations are cumbersome and underpowered, especially compared to libraries
  like
  [streaming]((https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:next))
  or
  [streaming-bytestring](https://hackage.haskell.org/package/streaming-bytestring).

## What about performance?

I haven't run any benchmarks, but you can safely assume that this library will
move like a snail compared to
[streamly](https://hackage.haskell.org/package/streamly)'s Ferrari.

## Some close cousins

- [turtle](https://hackage.haskell.org/package/turtle). The `Shell` type
  resembles `Jet`. One possible difference is that `Shell` doesn't seem to
  provide a way for the `Shell` consumer to signal that no further values are
  needed, at least judging from the docs for
  [limit](https://hackage.haskell.org/package/turtle-1.5.22/docs/Turtle-Prelude.html#v:limit).

  \"turtle\" also inspired the idea of having a separate type for lines.

- [streamly](https://hackage.haskell.org/package/streamly). I might have
  reinvented a subset of streamly ([but
  worse](https://www.mcmillen.dev/language_checklist.html)).

- [Z.IO.BIO](https://hackage.haskell.org/package/Z-IO-1.0.0.0/docs/Z-IO-BIO.html)
  from [Z-IO](https://hackage.haskell.org/package/Z-IO). Like `Jet`, uses a
  callback-transformation approach. 

- The
  [Stream](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/stream/Stream.html)
  type from Java is somewhat similar to this library's `Jet`. (And the
  [foldl](https://hackage.haskell.org/package/foldl) library would be
  the analogue of
  [Collectors](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/stream/Collectors.html).)

