# kibit-mode

kibit-mode is a combination of a thin wrapper around the excellent
[kibit](https://github.com/jonase/kibit) tool, and a minor for Clojure
buffers that will help you find ways to make your Clojure code more
idiomatic.

## Requirements

[Emacs](http://www.gnu.org/software/emacs/) 24.0 or greater
[Clojure](http://clojure.org) 1.4 or greater
[Leiningen](https://github.com/technomancy/leiningen) 1.6.2 or greater
[clojure-mode](https://github.com/technomancy/clojure-mode) 1.11.5 or greater

## Configuration

I use el-get to manage my Emacs config. To add kibit-mode, I added the
following to my package definitions:

```
(:name kibit-mode
       :type git
       :url "https://github.com/aredington/kibit-mode.git"
       :after (lambda ()
                (require 'kibit-mode)
                (add-hook 'clojure-mode-hook 'kibit-mode)))
```

The important bits are to get kibit-mode.el on your load-path, require
it, and add it as a hook to clojure-mode. Hopefully this works for you
if you also use el-get, if you do not use el-get you are on your own.

## Usage

In an open Clojure buffer, hit C-c C-n (you can use the mnemonic that
this will help you catch *N*oob mistakes) to open a compilation buffer
that will tell you where you can replace code with a terser, more
idiomatic expression of the same semantics.

## License

Copyright (C) 2012 Alex Redington

Distributed under the MIT License
