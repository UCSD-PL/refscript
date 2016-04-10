# refscript

Refinement Types for TypeScript

## Install

### Dependencies

* [stack](https://github.com/commercialhaskell/stack) The Haskell Tool Stack
* [Node.js](http://nodejs.org/) (tested version v4.2.1)
* [Ocaml](http://caml.inria.fr/ocaml/release.en.html)
* [Z3 Binary](http://z3.codeplex.com/) version >= 4.3.2

### Download and Build

    git clone https://github.com/UCSD-PL/refscript
    cd refscript
    git submodule init
    git submodule update
    stack setup
    stack build


## Run

### Basic Usage

    stack exec -- rsc /path/to/file.ts

### Regression testing

    stack test

### Run Benchmarks

    stack test refscript:bench

### Fast load (using ghci)

    stack ghci refscript:exe:rsc
    *main> top "file.ts" False


## Advanced

### Building with profiling support (uses cabal sandboxes)

To build with profiling support it is recommended that a new sandbox is used, as all library dependencies will have to be compiled with profiling support.

To do so, while in `$ROOT/RefScript`:

    mv .cabal-sandbox .cabal-sandbox.backup
    mv cabal.sandbox.config cabal.sandbox.backup.config

Then repeat the first steps of installation:

    cabal sandbox init
    cabal sandbox add-source ../liquid-fixpoint

This will create fresh `.cabal-sandbox` and `cabal.sandbox.config`

But before building, add the following option in `cabal.sandbox.config`:

    library-profiling: True
    executable-profiling: True

In addition, in `refscript.cabal` replace line:

    ghc-options:         -W -O2

with:

    ghc-options:         -W -O2 -prof -auto-all

Then build with:

    cabal install -p

This will build all depending libraries with profiling support.

To run `rsc` in profiling mode add flags `+RTS -p` in the end:

    rsc input.ts +RTS -p

More detailed options can be found [here](https://www.haskell.org/ghc/docs/7.8.3/html/users_guide/profiling.html).

If you're interested in profiling the evaluation of a specific expression you can add a *cost center annotation*:

    {-# SCC "name" #-} <expression>

What this command outputs is a file called `rsc.prof` that contains all gathered profiling information, including information about both all functions (default cost centers) and user defined cost centers.




## Editor Integration

We have some support for `rsc` in vim and emacs.

### Emacs

There is a `flycheck` plugin for RefScript.

1. Copy `ext/emacs/typescript-rsc.el` into your emacs PATH.

2. Add this to your `init.el`

    (require 'typescript)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (require 'flycheck-rsc)


### Vim

**Install**

1. Add the following to your `.vimrc`

~~~~~
Bundle 'scrooloose/syntastic'
Bundle 'panagosg7/vim-annotations'
~~~~~

2. Copy the following files

~~~~~
cp ext/vim/typescript/rsc.vim  ~/.vim/bundle/syntastic/syntax_checkers/typescript/rsc.vim
cp ext/vim/typescript/rsc.vim  ~/.vim/bundle/syntastic/syntax_checkers/typescript/rsc.vim
~~~~~

**Run**

+ `:SyntasticCheck liquid` runs `rsc` on the current buffer.

**View**

1. **Warnings** will be displayed in the usual error buffer.

2. **Inferred Types** will be displayed when `<F1>` is pressed over an identifier.


**Options**

You can configure the checker in various ways in your `.vimrc`.

+ To run after **each save**, for *all* Haskell files, add:

~~~~~
let g:syntastic_mode_map = { 'mode': 'active' }
let g:syntastic_typescript_checkers += ['liquid']
let g:syntastic_javascript_checkers += ['liquid']
~~~~~

+ To pass extra options to `rsc` add: 

~~~~~
let g:syntastic_typescript_liquid_args = '...'
~~~~~


### Notes

+ Using `export let a = ... ` brings in the inferred type from TypeScript.
