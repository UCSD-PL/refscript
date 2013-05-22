README
=======

Language for experimenting with verification algorithms

nano-js is the basis for the programming assignments in 

    http://goto.ucsd.edu/~rjhala/classes/sp13/cse291

Dependencies
------------

* git clone git@github.com:ucsd-progsys/liquid-fixpoint.git 
* git clone git@github.com:UCSD-PL/language-ecmascript.git
* nano-js

Homework Plan
-------------

HW 1
1a. VCG 
1b. Use ESC/J

HW 2
2a. ConsGen = VCG+K for LoopInv via FIXPOINT    [Easy]
2b. Implement FIXPOINT (over liquid-fixpoint)   [Hard]

HW 3
3a. VCG for Refinement Type Checking            [Hard]
3b. Consgen = VCG+K for Liquid Inference via FIXPOINT

Liquid Nano-JS
--------------

Failed 2 tests: 
    
    liquid/pos/driver.js,   <<------------ HEREHEREHEREHERE
                                           "random()" gets a FRESH type
                                           which is then "FALSED" out...
                                           only FRESH for funs with code...

    liquid/pos/locks-fun.js, 


* Bells and whistles
    + Scrape Qualifiers
    + list
    + array
    - Records

Tests
-----

tests/typed/pos/*.js

    higher-order
        id.js   
        twice.js
        forloop.js

    ------------ STOPPED 

    lists
        listsum.js
        range.js   
        map.js
        fold.js

        (List A -- ADT)
            single :: forall A. (A) => (List A)
            head   :: forall A. (List A) => A
            tail   :: forall A. (List a) => (List A)

    measure
        kmeans.js

    measures: 
        cons :: forall A. (x:A, xs:List A) -> {v: List A | (len v) = 1 + (len xs)}
        nil  :: forall A. () -> {v: List A | (len v) = 0}
        head :: forall A. (xs:{v: List A | 0 < (len v)}) -> A 
        tail :: forall A. (xs:{v: List A | 0 < (len v)}) -> {v: List A | (len v) = (len xs) - 1 }
        null :: forall A. (xs:List A) -> {v: Bool | (Prop v) <=> ((len v) = 0) }


Include
-------

/*@ include "path/to/foo.js" */
  >> add to "Spec"
  >> update parser
  >> recursively traverse all files
          traverseFiles :: (FilePath -> IO [FilePath]) -> FilePath -> IO [FilePath]


