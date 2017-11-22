# literate-purescript [![Build Status](https://travis-ci.org/Thimoteus/literate-purescript.svg?branch=master)](https://travis-ci.org/Thimoteus/literate-purescript)

## literate programming

Essentially, literate programming inverts the importance of code and comments.
Whereas in normal code, you need to identify comments (in purescript by a --
or {- -} for single/multi-line comments, respectively), in literate programming
you need to identify code.

## usage

Create a directory for your literate files.
Comments are normal lines of text, and code is marked by triple backticks.

Options are as follows:

```
Usage: litps [options]
     | litps compile [options]

Root options:
  --help -h       Shows this text.
  --version -v    Shows version.
  --file <path>   Specify only a single file as input.
  --input <dir>   Specify the directory of literate files.
                  Defaults to "literate/".
  --output <dir>  Specify the directory of output PS files.
                  Defaults to "src/".
  --i-ext <ext>   Specify the extension of literate files.
                  Defaults to "md".
  --o-ext <ext>   Specify the extension of source files.
                  Defaults to "purs".

Compile options: Run 'litps compile --help'
```

As of now the output directory must not exist (this is to prevent accidentally
overwriting source code, as actually happened to me during development). This
may change in a future version to allow interactively choosing whether to remove
the output directory before building or not.

## building

`npm run build`

This will create `litps` in the directory, which you can call: `node litps`.

## installing

`npm run copybin` will run `npm run build`, then mark it executable and move it
to `~/bin/` which should be in your `$PATH`.

## extra

Windows and MacOS are unsupported.