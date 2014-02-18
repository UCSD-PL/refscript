### Language for experimenting with verification algorithms

## Dependencies

NanoJS depends on the following projects:

 * Haskell compiler
 * Ocaml Compiler
 * [Node.js](http://nodejs.org/) and [Jake](https://github.com/mde/jake)
 * [Liquid-fixpont](https://github.com/ucsd-progsys/liquid-fixpoint): Haskell Interface for Back-End Implication / Horn Clause Constraint Solving for Liquid Types.
 * [Language Ecmascript] (https://github.com/UCSD-PL/language-ecmascript): JavaScript Syntax writen in Haskell.
 * [TypeScript](https://github.com/panagosg7/typescript): TypeScript compiler, used as front-end.



## Installation

The following instructions have been tested on Ubuntu >= 12.04. 

We assume that the requirements regarding the Haskell and Ocaml compiler are met.


### Installing Node.js and Jake 

TypeScript depends on:

+ [node.js](http://nodejs.org/) version > 0.8 
+ [jake](https://github.com/mde/jake)


#### Installing Node/Jake on MacOS

1. Install the [node package](http://nodejs.org/)
2. `sudo npm install -g jake`

#### Installing Node/Jake on Ubuntu/Debian

    sudo apt-get install nodejs npm

In Ubuntu 12.04 the distributed version of Node is too old, so you may need to
install a newer version from the following PPA:

    sudo add-apt-repository ppa:chris-lea/node.js
    sudo apt-get update
    sudo apt-get install nodejs

Then:

    sudo npm install -g jake

or, if this fails:

    sudo npm install -g jake --registry http://registry.npmjs.org/



### Get source code

Clone all dependencies and NanoJS in the same directory `ROOT`:

    git clone https://github.com/ucsd-progsys/liquid-fixpoint
    git clone https://github.com/UCSD-PL/language-ecmascript
    git clone https://github.com/panagosg7/typescript
    git clone https://github.com/UCSD-PL/nano-js

After acquiring the code you should be presented with the following file structure:

```
ROOT
  ├── language-ecmascript
  ├── liquid-fixpoint
  ├── nano-js
  └── typescript
```
    
### Create Virtual Haskell Environment (Optional)

It is recommended to use an isolated Haskell environment (https://github.com/Paczesiowa/hsenv). Create one using version 7.6.3 of the GHC Haskell compiler (available [here](https://www.haskell.org/ghc/download_ghc_7_6_3)):

    hsenv --ghc=/PATH/TO/GHC/ghc-7.6.3-x86_64-unknown-linux.tar
    
And then to enable it:

    source .hsenv/bin/activate
    
    

### Build dependencies

    cd liquid-fixpoint     && cabal install && cd ..
    cd language-ecmascript && git checkout heapless_parse_json && cabal install && cd ..
    cd typescript          && jake


    
### Build NanoJS

    cd nano-js             && git checkout heapless_classes && cabal install && cd ..



## Usage

To run NanoJS on a single TypeScript file:

    nanojs liquid /path_to_JS_file/file.ts




## Specifications

### Signatures

TODO


### Type Invariants

You can write type-invariants like:

    invariant {v:number    | ttag(v) = "number"   }
    invariant {v:number    | ttag(v) = "number"   }
    invariant {v:undefined | ttag(v) = "undefined"}
    invariant {v:null      | ttag(v) = "object"   }
    invariant {v:boolean   | ttag(v) = "boolean"  }  
    invariant {v:number    | ttag(v) = "number"   } 
    invariant {v:string    | ttag(v) = "string"   } 

These invariants are automatically used to strengthen the refinements
for values of the relevant types.

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

### Data Type Declarations

You can write data type declarations in the follownig way:

    /*@ type list[A]       = { data: A, next: list[A] } */

