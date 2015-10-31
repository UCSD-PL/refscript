# refscript

Refinement Types for TypeScript

## Install

### Dependencies

* [ghc](http://www.haskell.org/ghc/) version 7.10.2
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


## Basic Usage

    stack exec -- rsc /path/to/file.ts

## Regression tetsing

Install `tasty` (if not present):

    stack install tasty

Run regression:

    make tasty

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




## Specifications

All specifications are added in comments of the following form:

    /*@ <specification> */

RefScript currently allows the following forms of specifications:

### Signatures

Every function, class method and class constructor needs to be annotated with a signature:

    /*@ foo :: T */
    function foo(x1,...,xn) { Body }

The form of the function signature `T` is described later.

For example:

    /*@ pos :: (n: number) => boolean */
    function pos(n) {
        return n > 0;
    }

Similarly for constructors and class methods:

    /*@ foo : T */
    public foo(x1,...,xn) { Body }

    /*@ constructor : T */
    constructor (x1,...,xn) { Body }


### Type Annotations

Type anntoations on variables are of the form:

    /*@ x :: t */
    var x [= <exp>];

For example:

    /*@ a :: { number | v > 0 } */
    /*@ b :: string */
    var a = 1,
        b = "foo",
        c = { f: "bar" };

Type annotations in variable declaration groups like the one above are optional, as opposed to ambient declarations:

    /*@ a :: number */
    declare var a: number;


Also, type annotations are necessary for class members:

    class A {
        /*@ a :: { number | v > 0} */
        public a: number;
        ...
    }

Define interfaces in a similar way:

    interface IA {
        /*@ a :: { number | v > 0} */
        a: number;
        ...
    }

### Type Qualifiers

You can write type qualifiers like this:

    /*@ qualif CmpZ(v:number) : v > 0 */

Several more examples can be found in `include/prelude.ts`.


### Type Invariants

Type *invariants* are predicates that are automatically added to the
refinement part of types of certain raw type. For example you can specify
the following:

    invariant {v:number    | ttag(v) = "number" }
    invariant {v:boolean   | ttag(v) = "boolean"}
    invariant {v:string    | ttag(v) = "string" }

### Aliases

You can write parameterized *predicate* and *type* aliases to abbreviate
commonly used types, for example:

    /*@ predicate gt x y  = x >= y          */

or

    /*@ predicate gt(x, y) = x >= y         */

defines a predicate alias. You can use this to write a type alias:

    /*@ alias gnat<A, x>   = {v: A | gt(v, x)}  */

or

    /*@ alias gnat[A,x]    = {v: A | gt(v, x)}  */

Note that

* _Upper_-case `A` is used for **type**  parameters,
* _Lower_-case `x` is used for **value** parameters.

Finally, you can use the above aliases to define:

    /*@ alias nat          = gnat<number, 0> */

or

    /*@ alias nat          = gnat[number, 0] */

You can use `nat` as an alias for: `{number | v >= 0}`

    /*@ z :: nat */
    var z = 12;

See `tests/pos/typealias/` for more examples.

## Grammar

#### Base Types

    b ::= number                                // Number
        | boolean                               // Boolean
        | string                                // String
        | void                                  // Void
        | top                                   // Τop type
        | null                                  // Null
        | undefined                             // Undefined
        | A                                     // Type variable
        | #<id>[t, ... ]                        // Named type
        | {f1: t1, f2: t2, ...}                 // Object type
        | [ t ]                                 // Array type
        | t + t + ...                           // Union type
        | b ?                                   // Optionally null type

The shorthand `t?` stands in for `t + null`.


#### Refined Types

    τ ::=  b
        | { b | p }
        | { v: b }
        | { v: b | p }


#### Function Types (signatures)

    σ  ::= (x1: t1, x2: t2, ...) => t           // Function type
         | forall A B ... . σ                   // Parametric function type
         | /\ σ /\ σ ...                        // Overloaded function type


#### Types

    t  ::= τ
         | σ


### Predicates

    p ::= true                                  // true
        | false                                 // false
        | p && p                                // Conjunction
        | P || p                                // Disjunction
        | ~ p                                   // Negation
        | p => p                                // Implication
        | p <=> p                               // Equivalenc
        | E bRel E                              // Binary relation

#### Binary relations

    bRel ::= =                                  // Equality
           | !=                                 // Inequality
           | ~~                                 // Unary equality   (???)
           | !~                                 //                  (???)
           | <                                  // Less than
           | <=                                 // Less than or equal
           | >                                  // Greater than
           | >=                                 // Greater than or equal


#### Expressions

    E ::= 1   | 2   ...                         // Integer constants
        | "a" | "b" ...                         // String constants
        | V                                     // Variables
        | ???                                   // ELit             (???)
        | E E                                   // Application
        | E bOp E                               // Binary operation
        | if E then E else E                    // If-then-else

TODO: some stuff is missing here. Do we even use that?


#### Binary operations

    bOp ::= +                                   // Plus
          | -                                   // Minus
          | *                                   // Multiplication
          | /                                   // Division
          | mod                                 // Modulo


#### Variables

    V ::= v | w | z ...







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
cp ext/vim/typescript/nanojs.vim  ~/.vim/bundle/syntastic/syntax_checkers/typescript/nanojs.vim
cp ext/vim/typescript/nanojs.vim  ~/.vim/bundle/syntastic/syntax_checkers/typescript/nanojs.vim
~~~~~

**Run**

+ `:SyntasticCheck liquid` runs `nanojs` on the current buffer.

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

+ To pass extra options to `nanojs` add:

~~~~~
let g:syntastic_typescript_liquid_args = '...'
~~~~~
