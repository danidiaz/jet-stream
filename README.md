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

- You can't detect the "end" of a stream inside the stream itself.

- Partly as a consequence of the previous point, grouping operations inside a
  stream are not well supported.  You can, however, perform some form of
  grouping as a terminal operation.

## Some close cousins

- [turtle](https://hackage.haskell.org/package/turtle). The `Shell` type kinda
  resembles `Jet`.

- [Z.IO.BIO](https://hackage.haskell.org/package/Z-IO-1.0.0.0/docs/Z-IO-BIO.html)
  from [Z-IO](https://hackage.haskell.org/package/Z-IO). Like `Jet`, uses a
  callback-transformation approach..

- The
  [Stream](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/stream/Stream.html)
  type from Java is somewhat similar to this library's `Jet`. (And the
  [foldl](https://hackage.haskell.org/package/foldl) library would be
  the analogue of
  [Collectors](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/stream/Collectors.html).)

