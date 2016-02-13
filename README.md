# literate-purescript [![Build Status](https://travis-ci.org/Thimoteus/literate-purescript.svg?branch=master)](https://travis-ci.org/Thimoteus/literate-purescript)

## literate programming

Essentially, literate programming inverts the importance of code and comments.
Whereas in normal code, you need to identify comments (in purescript by a --
or {- -} for single/multi-line comments, respectively), in literate programming
you need to identify code.

## usage

Create a directory for your literate files. Write all your comments in non-indented
lines, and mark code lines by Bird tracks: `> ` (the space is mandatory).

Run the transpiler on the directory. It will generate `.purs` source files with
the comments removed into your `src/` directory.

You can build the transpiler with `npm run build`.

## example

This tool is self-hosting, so the "real" source is in the `litps/` directory.
Earlier versions of the transpiler are run against it:
```
> node transpiler litps/
```
which creates purescript source files in `src/`, which are then built using Pulp.
