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

* Bells and whistles
    
    + list
    + array
    + HTML annot
    + Scrape Qualifiers
    
    - Records

Tests
-----
    
    higher-order
        forloop.js

    ------------ STOPPED 

    lists
        listsum.js
        range.js   
        map.js
        fold.js

        (List A -- ADT)
            cons  :: forall A. (A, list A)   => list A
            nil   :: forall A. ()            => list A
            head  :: forall A. (list A)      => A
            tail  :: forall A. (list a)      => (list A)
            nth   :: forall A. (int, list A) => A
            empty :: forall A. (xs:list A)   => {v: boolean | (Prop v) <=> ((len v) = 0) }

    arrays
        DOTPROD?
        KMP?
        mapreduce/kmeans?

    measure
        kmeans.js

    measures: 
        cons  :: forall A. (x:A, xs:List A) -> {v: List A | (len v) = 1 + (len xs)}
        nil   :: forall A. () -> {v: List A | (len v) = 0}
        head  :: forall A. (xs:{v: List A | 0 < (len v)}) -> A 
        tail  :: forall A. (xs:{v: List A | 0 < (len v)}) -> {v: List A | (len v) = (len xs) - 1 }
        empty :: forall A. (xs:List A) -> {v: Bool | (Prop v) <=> ((len v) = 0) }
        

    


Include
-------

/*@ include "path/to/foo.js" */
  >> add to "Spec"
  >> update parser
  >> recursively traverse all files
          traverseFiles :: (FilePath -> IO [FilePath]) -> FilePath -> IO [FilePath]


