README
=======

Language for experimenting with verification algorithms

nano-js is the basis for the programming assignments in 

    http://goto.ucsd.edu/~rjhala/classes/sp13/cse291


Install
=======

Dependencies
------------

* git clone git@github.com:ucsd-progsys/liquid-fixpoint.git 
* git clone git@github.com:UCSD-PL/language-ecmascript.git
* git clone git@github.com:UCSD-PL/nano-js.git

Build
-----

Execute the following:

    cd liquid-fixpoint     && cabal install && cd ..
    cd language-ecmascript && cabal install && cd ..
    cd nano-js             && cabal install && cd ..

Specifications
==============


Signatures
----------

TODO


Type Invariants
---------------

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



Tests
-----

    DOTPROD?
    KMP?
    mapreduce?
    kmeans?

