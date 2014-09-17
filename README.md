# RefScript

Refinement Types for Scripting Languages

## Install

### Dependencies

RefScript requires: 

* [ghc](http://www.haskell.org/ghc/) version 7.8.3 
* [cabal](http://www.haskell.org/cabal/) version > 1.18
* [Node.js](http://nodejs.org/) version > 0.8 
* [Ocaml](http://caml.inria.fr/ocaml/release.en.html)
* [Z3 Binary](http://z3.codeplex.com/) version >= 4.3.2

### Installing Node on Ubuntu/Debian

    sudo apt-get install nodejs npm

In Ubuntu 12.04 the distributed version of Node is too old, so you may need to
install a newer version from the following PPA:

    sudo add-apt-repository ppa:chris-lea/node.js
    sudo apt-get update
    sudo apt-get install nodejs npm

### Download and Build 

Suppose you want to work in a directory `$ROOT`. First `cd $ROOT`, and then:

    git clone https://github.com/panagosg7/liquid-fixpoint
    git clone https://github.com/UCSD-PL/language-ecmascript
    cd RefScript && git checkout RefScript && cd ..
    git clone https://github.com/UCSD-PL/RefScript
    cd RefScript && git checkout develop && cd ..
    cd RefScript
    cabal sandbox init
    cabal sandbox add-source ../liquid-fixpoint
    cabal sandbox add-source ../language-ecmascript
    make
	

### Building typescript (optional) 

You can **optionally** get the latest version of our TypeScript to RefScript
translation phase, as an alternative to using the precompiled scripts in
`RefScript/ext/tsc-bin`. In this case you will have to get the sources:

    git clone https://github.com/panagosg7/typescript

After acquiring the code you should have the following folder structure:

    ROOT
      ├── language-ecmascript
      ├── liquid-fixpoint
      ├── RefScript
      └── typescript

If you downloaded the TypeScript to RefScript translation, you can now build it: 

    cd typescript          && jake

To use this version of `tsc` later on, make sure that tye `./typescript`
directory is in the environment under `TSC_ROOT`. You can can achieve this with:

    export TSC_ROOT=$PWD

Then head back to the ROOT directory:

    cd ..


Building `tsc` requires [jake](https://github.com/mde/jake). In case jake is not
available with your nodejs installation, you can install it in the following
way:

    sudo npm install -g jake

or, if this fails:

    sudo npm install -g jake --registry http://registry.npmjs.org/



## Usage

To run RefScript on a single TypeScript file:

    rsc /path/to/file.ts


## Specifications

All specifications are added in comments of the following form:

    /*@ <specification> */

RefScript currently allows the following forms of specifications:

### Signatures

Every function, class method and class constructor needs to be annotated with a signature:

    /*@ <id> :: σ */
    function <id> (args...) { Body }

The form of the function signature `σ` is described later.

For example:

    /*@ pos :: (n: number) => boolean */
    function pos(n) {
        return n > 0;
    }

Similarly for constructors and class methods:

    /*@ <id> :: σ */
    public <id> (args...) { Body }
    
    /*@ constructor :: σ */
    constructor (args...) { Body }



### Type Annotations

Type anntoations on variables are of the form: 

    /*@ <id> :: t */
    var <id> [= <exp>];

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

    invariant {v:number    | ttag(v) = "number"   }
    invariant {v:boolean   | ttag(v) = "boolean"  }  
    invariant {v:string    | ttag(v) = "string"   } 



### Aliases

You can write parameterized *predicate* and *type* aliases to abbreviate
commonly used types, for example:

    /*@ predicate gt x y  = x >= y          */

or
   
    /*@ predicate gt(x, y) = x >= y         */

defines a predicate alias. You can use this to write a type alias:

    /*@ alias gnat[A,x]    = {v: A | (gt v x)}  */

or 
    
    /*@ alias gnat[A,x]    = {v: A | gt(v, x)}  */

Note that 

* upper-case `A` is used for **type**  parameters,
* lower-case `x` is used for **value** parameters.

Finally, you can use the above aliases to define:

    /*@ alias nat          = gnat[number, 0] */

and now use `nat` as an alias for: `{number | v >= 0}`

    /*@ z :: nat */
    var z = 12;




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
