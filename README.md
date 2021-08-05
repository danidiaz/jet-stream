# jet-stream

This is yet another streaming library for Haskell, created to scratch the
following itches:

- The main type is as simple as possible: the only type parameter is the type
  of the yielded elements.

- The `Monoid` / `Alternative` / `MonadPlus` methods perform concatenation,
  just like with regular lists. The `Functor` `Applicative` and `Monad`
  instances also resemble those of lists.

- Typical "control" functions like `withFile`, `bracket`, `finally` and
  `onError` are easy to integrate in a streaming pipeline. 

- Compatible with the [foldl](https://hackage.haskell.org/package/foldl)
  library for collector-like terminal operations. (All self-respecting
  streaming libraries must have this.)

In order to achieve those objectives, the following sacrifices have been made:

- No flexibility in the underlying monad for the stream effects: it's always
  `IO`.

- Elements in a stream can't be "extracted" one by one in a pull-based way,
  like you can do for example in
  [streaming](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:next).

- There are `take` and `drop` operations, but no `splitAt` (in contrast with
  [streaming](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:next)
  which represents
  [`splitAt`](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:splitAt)
  very elegantly). 

- Grouping operations are underpowered (In fact, they don't exist right now.)

## What about performance?

I haven't run any benchmarks, but you can safely assume that this library will
move like a snail compared to
[streamly](https://hackage.haskell.org/package/streamly)'s Ferrari.

## Some ghci examples

Invoking an external process:

    ghci> mempty & throughProcess defaults (shell "echo foo") & sink stdout

A concurrent pipeline:

    ghci> each @Int [1,2,3] & traverseConcurrently id (\a -> threadDelay 2e6 *> return (2^a)) <&> show <&> T.pack & funnel stdout

A concurrent pipeline with more workers:

    ghci> each @Int [1,2,3] & traverseConcurrently (numberOfWorkers 3) (\a -> threadDelay 2e6 *> return (2^a)) <&> show <&> T.pack & funnel stdout

A concurrent pipeline that limits its outputs:

    ghci> each @Int [1,2,3] & traverseConcurrently id (\a -> (threadDelay 2e6 *> return (2^a))) & Jet.limit 2 <&> show <&> T.pack & funnel stdout

To demonstrate that pending jobs are killed when we the a limit:

    ghci> each @Int [1,2,3] & traverseConcurrently id (\a -> (threadDelay 2e6 *> return (2^a)) `Control.Exception.onException` (print (show a ++ " was canceled"))) & Jet.limit 2 <&> show <&> T.pack & funnel stdout

## Some close cousins

- [turtle](https://hackage.haskell.org/package/turtle). The `Shell` type kinda
  resembles `Jet`. One possible difference is that `Shell` doesn't seem to
  provide a way for the `Shell` consumer to signal that no further values are
  needed, at least judging from the docs for
  [limit](https://hackage.haskell.org/package/turtle-1.5.22/docs/Turtle-Prelude.html#v:limit).

  \"turtle\" also inspired having a separate type for lines.

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

