Jacinda is a functional pattern filtering language,
inspired by [AWK](http://www.awklang.org).

# Installation

## Releases

There are binaries for some platforms on the [releases page](https://github.com/vmchale/jacinda/releases/).

If you are on Mac, you will need to install `*-librure.dylib` as well.

## Nix

Jacinda is available as a [Nix package](https://search.nixos.org/packages?channel=unstable&from=0&size=1&type=packages&query=jacinda) (thanks to Lukas Epple, who is the maintainer).

## From Source

First, install [Rust's regex library](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine). You'll need to put `librure.so` or `librure.dylib` etc. in the appropriate place.

If you have [cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) installed (perhaps via [ghcup](https://www.haskell.org/ghcup/)):

```
cabal install jacinda
```

## Editor Support

There is a [vim plugin](https://github.com/vmchale/jacinda-vim) and a [VSCode extension](https://marketplace.visualstudio.com/items?itemName=vmchale.jacinda).

# Usefulness

Unix uses record separators in many places; we can display one entry in the
`PATH` variable with:

```
echo $PATH | ja -F: "{|[x+'\n'+y]|>\`$}"
```

Many Unix tools output information separated with spaces. We use regular
expressions to match relevant lines and then select the field with the data
itself, viz.

```
otool -l $(locate libpng.dylib) | ja '{`1 ~ /^name/}{`2}'
```

To get the value of a variable (say, `PATH`) from the output of `printenv`:

```
printenv | ja -F= '{%/^PATH/}{`2}'
```

# Documentation

See the [guide](https://vmchale.github.io/jacinda/) (archived on [Hackage](https://hackage.haskell.org/package/jacinda/src/doc/guide.pdf)), which contains a tutorial
on some of the features as well as examples.

The [manpages](https://hackage.haskell.org/package/jacinda/src/man/ja.1) document the builtins and provide a syntax reference.

See the [rosetta](./ROSETTA.md) showing AWK alongside, or the [examples](./EXAMPLES.md) for its own capabilities.

# Status

## Missing Features & Bugs

  * No nested dfns
  * No list literal syntax
  * Postfix `:f` and `:i` are handled poorly
  * Streams of functions don't work
  * Higher-order functions are subtly broken

Intentionally missing features:

  * No loops

# Advantages

  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support
  * Deduplicate builtin

# Contributing

I have rewritten the code several times so forking and applying patches is fraught!

Bug reports are welcome contributions.
