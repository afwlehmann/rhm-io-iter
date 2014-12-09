# Introduction to Iteratees in Haskell

This is the demo code for a talk about Iteratees held at the Regensburg Haskell
Meetup at December 3rd, 2014. It consists of implementations of Iteratees both
pure and as monad transformers, along with a small set of examples that
demonstrate various features.

You might want to start with looking at the examples in `LazyIO` which
demonstrate two well-known problems with lazy IO in Haskell. After that have a
look at the implementation of pure Iteratees in `RHM.Intern.Pure` as well as the
corresponding examples to be found in the `RHM.Iteratee` and the `Main` module.

Note that the pure implementation is largely (if not at all) redundant when the
transformer variant is used instead (see e.g. `StateT` and `State`).

## Running the examples

Install the project's dependencies into a cabal sandbox:

```sh
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Quickly browse through the examples in `src/LazyIO.hs` and run the demo with

```sh
cabal run lazyio-demo FILENAME
```

Replace `FILENAME` with an existing file (will be opened read-only). Adapt the
code to run other examples.

Run the iteratee demo with

```sh
cabal run iter-demo
```

Either adapt the code or (preferably) start a REPL session with `cabal repl`.

## Other resources:
- John W. Lato: "Iteratee: Teaching an Old Fold New Tricks", Monad Reader #16<br/>
  http://themonadreader.files.wordpress.com/2010/05/issue16.pdf

- Oleg Kiselyov:
  - Web root:<br/>
    http://okmij.org/ftp/Streams.html

  - "Iteratees"<br/>
    http://okmij.org/ftp/Haskell/Iteratee/describe.pdf

  - Annotated slides corresponding to the talk at FLOPS 2012<br/>
    http://okmij.org/ftp/Haskell/Iteratee/talk-FLOPS.pdf

  - "Lazy IO breaks equational reasoning"<br/>
    http://okmij.org/ftp/Haskell/index.html#lazyIO-not-True

- Edward Kmett: "A Parsing Trifecta", Boston Area Haskell User Group 2009<br/>
  http://comonad.com/reader/2009/iteratees-parsec-and-monoid/
