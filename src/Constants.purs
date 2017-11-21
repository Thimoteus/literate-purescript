module Constants where

helpText :: String
helpText = """Usage: litps [options]
     | litps compile [options]

Root options:
  --help -h       Shows this text.
  --version -v    Shows version.
  --file <path>   Specify only a single file as input.
  --input <dir>   Specify the directory of literate files.
                  Defaults to "litps/".
  --output <dir>  Specify the directory of output PS files.
                  Defaults to "src/".
  --i-ext <ext>   Specify the extension of literate files.
                  Defaults to "md".
  --o-ext <ext>   Specify the extension of source files.
                  Defaults to "purs".

Compile options: Run 'litps compile --help'
"""

helpCompile :: String
helpCompile = """Usage: litps compile [options]

Compile options:
  --help -h       Shows this text.
  --file <path>   Specify only a single file as input.
  --input <dir>   Specify the directory of literate files.
                  Defaults to "litps/".
  --output <dir>  Specify the directory of output PS files.
                  Defaults to "src/".
  --i-ext <ext>   Specify the extension of literate files.
                  Defaults to "md".
  --o-ext <ext>   Specify the extension of source files.
                  Defaults to "purs".
"""

versionText :: String
versionText = "0.1.0"