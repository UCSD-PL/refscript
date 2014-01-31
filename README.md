#### Language for experimenting with verification algorithms

## Install

### Dependencies:

    git clone git@github.com:ucsd-progsys/liquid-fixpoint.git 
    git clone git@github.com:UCSD-PL/language-ecmascript.git
    git clone git@github.com:UCSD-PL/nano-js.git

### Build:

    cd liquid-fixpoint     && cabal install && cd ..
    cd language-ecmascript && cabal install && cd ..
    cd nano-js             && cabal install && cd ..

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

    /*@ type gnat[A,x]    = {v: A | (gt v x)}  */

or 
    
    /*@ type gnat[A,x]    = {v: A | gt(v, x)}  */

Note that 

* upper-case `A` is used for **type**  parameters,
* lower-case `x` is used for **value** parameters.

Finally, you can use the above aliases to define:

    /*@ type nat          = gnat[number, 0] */

and now use `nat` as an alias for: `{number | v >= 0}`

    /*@ z :: nat */
    var z = 12;
