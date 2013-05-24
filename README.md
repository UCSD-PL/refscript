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

HW 2 Release Checklist
----------------------

* Add Refts to Functions

* Add FRESH binders to all parsed, nameless refinements.

* Fix: tests/liquid/pos/safeList.js

* Remove all GHC make "warnings"

* Make hw-liquid branch 

    - NUKE ESC
    - EDIT Liquid.hs
    - EDIT tests (remove quals/spec)

* Copy over to algo-verif repo

* Update GOTO haddocks
    
    - liquid-fixpoint
    - nano-js [generate from algo-verif-repo]
    
* RELEASE

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
    + HTML annot
    
    - Scrape Qualifiers
    - Records

Tests
-----
    
    arrays
            DOTPROD?
            KMP?
            mapreduce/kmeans?
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

HashMap.Strict Container MADNESS
--------------------------------

    tests/liquid/pos/minindex01.js

grumble about "unbound variable" (due to missing key in envFindTy)

    sometimes it works with "forloop" sometimes doesn't!
    when it doesn't if you change the name to "forLoop" or
    "humphreyAppleby" it works fine!

    using: 
        ~/research/liquid/hsenv
        hashable-1.2.0.7

    you get the error in:
        tests/liquid/pos/locks-cond.js



Failed 7 tests: liquid/neg/listsum-bad.js, liquid/neg/minindex02-bug.js, liquid/pos/listsum.js, liquid/pos/minindex00.js, liquid/pos/minindex01.js, liquid/pos/minindex02.js, liquid/pos/safeLists.js

Exceptions thrown on 6 tests: liquid/neg/listsum-bad.js, liquid/neg/minindex02-bug.js, liquid/pos/listsum.js, liquid/pos/minindex00.js, liquid/pos/minindex01.js, liquid/pos/minindex02.js



