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



Homework Plan
=============

HW 1
1a. VCG 
1b. Use ESC/J

HW 2
2a. ConsGen = VCG+K for LoopInv via FIXPOINT    [Easy]
2b. Implement FIXPOINT (over liquid-fixpoint)   [Hard]

HW 3
3a. VCG for Refinement Type Checking            [Hard]
3b. Consgen = VCG+K for Liquid Inference via FIXPOINT

Tests
-----

    DOTPROD?
    KMP?
    mapreduce?
    kmeans?

