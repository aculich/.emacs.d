# My personal Emacs configuration #

Based heavily on (e.g. forked directly from) Sebastian Wiesner's awesome Emacs setup: https://github.com/lunaryorn/.emacs.d

With many awesome additions (such as the most-excellent [skewer-mode](https://github.com/skeeto/.emacs.d#skewer)) taken from Christopher Wellons's configs: https://github.com/skeeto/.emacs.d


Makes extensive use of [use-package](https://github.com/jwiegley/use-package).

Also, I prefer using [Cask](https://github.com/cask/cask) to install packages rather than rely on use-package to download and byte-compile the packages. When dealing with a large number of packages it can be frustrating to have one byte-compilation snafu that aborts the initial (often lengthy) first-time setup process–– forcing you to restart the process all over again.

## Setup — How do you use it? ##

You need Emacs 25 snapshot builds, straight from Git `master`.  Stable releases of GNU Emacs won’t work, I build GNU Emacs weekly.

```console
$ git clone https://github.com/lunaryorn/.emacs.d.git ~/.emacs.d
$ brew install trash coreutils
$ brew install aspell --with-lang-en
$ cd ~/.emacs.d && cask install
```

plus all the standard tools for all the various programming languages (`sbt`, `hlint`, `stack`, `pandoc`, `pylint` and stuff).  Read the comments in `init.el` for more information.

## Layout — Where do you find things? ##

It’s a single big `init.el`, containing only `use-package` declarations for all the built-in and 3rd party packages I use.  These declarations have all the configuration and setup for the specific packages.

There’s also `lisp/` which has many of [lunaryorn’s](https://github.com/lunaryorn/.emacs.d) personal extensions and libraries with custom functions, etc.  These libraries are loaded like normal packages with `use-package` in `init.el`. Much of the code is useful and I would like to generalize it in the future, but for now it contain’s the lunaryorn namespace prefix.

## Highlights — What you should probably copy from this! ##

- Very good OS X support, even with stock GNU Emacs, including a font setup that supports Math, Symbols and Coloured Emojis (yay 😍)
- A well-designed key bindings scheme, inspired by [Spacemacs][], supported by
  [Which Key Mode][] and [Hydra][]
- Rules for buffer displays in `display-buffer-alist`
- A very powerful and comprehensive LaTeX setup with [AUCTeX][]
- Nice configurations for Scala (with [Ensime][]), Emacs Lisp and a couple of
  other languages
- Built-in web server you can launch with `httpd-start` and stop it with `httpd-stop`. It will serve files from the directory at `httpd-root` and log messages in s-expression form to the `*httpd*` buffer.
- [Skewer][] provides a JavaScript REPL (`skewer-repl`) and live expression evaluation in JavaScript buffers of an external web browser attached to the built-in web server running in Emacs. It can also do live "evaluation" of CSS (`skewer-css-mode`) and HTML (`skewer-html-mode`).


[Spacemacs]: http://spacemacs.org
[Which Key Mode]: https://github.com/justbur/emacs-which-key
[Hydra]: https://github.com/abo-abo/hydra
[AUCTeX]: https://www.gnu.org/software/auctex/
[Ensime]: http://ensime.github.io
[Skewer]: https://github.com/skeeto/.emacs.d#skewer

## Final words ##

Have fun, and copy freely!  Please feel free to mess around, and take whatever
you like!  Credits mandatory (respect the license), feedback appreciated!

If you’ve got any questions about this configuration or have a cool extension to
share please [open an issue](https://github.com/aculich/.emacs.d/issues/new). If the issue is related to an upstream source I will check in upstream with [skeeto](https://github.com/skeeto/.emacs.d/issues) or [lunaryorn](https://github.com/lunaryorn/.emacs.d/issues).

## License ##

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU
Emacs; see the file COPYING.  If not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
