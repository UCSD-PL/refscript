#### Language for experimenting with verification algorithms

## Install

### Dependencies

    git clone git@github.com:ucsd-progsys/liquid-fixpoint.git 
    git clone git@github.com:UCSD-PL/language-ecmascript.git
    git clone git@github.com:UCSD-PL/nano-js.git

### Build

    cd liquid-fixpoint     && cabal install && cd ..
    cd language-ecmascript && cabal install && cd ..
    cd nano-js             && cabal install && cd ..


## Run

To run NanoJS on a single file:

    nanojs liquid /path_to_JS_file/input.js


## TypeScript to NanoJS translation

You can translate annotated [TypeScript](http://www.typescriptlang.org/)
programs to NanoJS by using
[TStoNanoJS](https://github.com/panagosg7/TStoNanoJS), a fork of the
TypeScript compiler that emits NanoJ-annotated code.

#### Dependencies

TypeScript depends on [Node] (http://nodejs.org/) version > 0.8 and `Jake`. 
Here are some instructions for installation in Ubuntu/Debian:

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

#### Build

To build TstoNanoJS from source:

    git clone https://github.com/panagosg7/TStoNanoJS
    cd TStoNanoTS
    jake

The target script will be in `build/local/tsc/js`.


#### Run

To run the translator:

    node build/local/tsc.js /path_to_TS_file/input.ts

This will produce an output in the same folder with the input, the same name,
but a JavaScript instead of a TypeScript extention.

You can find a suite of examples in `tests/nanojs/misc/pass`.



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

