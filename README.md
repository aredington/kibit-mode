# kibit-mode

kibit-mode is a combination of a thin wrapper around the excellent
[kibit](https://github.com/jonase/kibit) tool, and a minor for Clojure
buffers that will help you find ways to make your Clojure code more
idiomatic.

## Requirements

* [Emacs](http://www.gnu.org/software/emacs/) 24.0 or greater
* [Clojure](http://clojure.org) 1.4 or greater
* [Leiningen](https://github.com/technomancy/leiningen) 1.6.2 or greater
* [clojure-mode](https://github.com/technomancy/clojure-mode) 1.11.5 or greater

## Configuration

### Via el-get
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

### Via package.el [melpa](http://melpa.milkbox.net/)

You can also install kibit-mode with package.el whichis built-in
package manager in Emacs 24+. If you use Emacs 23, you can get
[package.el](http://bit.ly/pkg-el23) yourself.

To install kibit-mode, you need this to your `~/.emacs.d/init.el`.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

And then you can install kibit-mode with the following command:

<kbd>M-x package-install [RET] kibit-mode [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file(`.emacs` or `init.el`):

```lisp
(when (not (package-installed-p 'kibit-mode)
  (package-install 'kibit-mode)
```

If the installation dosen't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

## Usage

In an open Clojure buffer, hit C-c C-n (you can use the mnemonic that
this **C**ompilation tool will help you catch **N**oob mistakes) to
open a compilation buffer that will tell you where you can replace
code with a terser, more idiomatic expression of the same semantics.

The compilation buffer will output formatted filename and line number
indicators of kibits suggestions, like follows:

```
/Users/alex/projects/pi/src/pi/core.clj:5:
  Replace
    (if (even? x) x nil)
  with
    (when (even? x) x)
```

You can jump immediately to the suggestion from the compilation buffer
by hitting enter. If you do not have any code that kibit thinks it can
improve, then it will exit happily and the compilation step will
report success.

## Reverse Double Secret Next Level Maneuver

If you add the following to your emacs config:

```
(add-hook 'clojure-mode-hook 'flymake-mode-on)
```

Then kibit-mode will be registered as a flymake checker and check your
code for you as you write. It will highlight the line which starts the
form relevant to kibit's suggestion. This part is pretty ugly and
hacked together with a shell script, be warned.

To use kibit-mode with
[flycheck](https://github.com/lunaryorn/flycheck), add the following to
your emacs config:

```
(add-hook 'clojure-mode-hook 'flycheck-mode)
```

## License

Copyright (C) 2012 Alex Redington

Distributed under the MIT License
