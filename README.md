README
=======

Language for experimenting with verification algorithms

nano-js is the basis for the programming assignments in 

    http://goto.ucsd.edu/~rjhala/classes/sp13/cse291

Strings
-------

- add a simple test
- update TC to support `string` type
- update constraint generation
- run
Dependencies
------------

* git clone git@github.com:ucsd-progsys/liquid-fixpoint.git 
* git clone git@github.com:UCSD-PL/language-ecmascript.git

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

Todo List
---------

- Scrape Qualifiers
- unions
- Records
- Objects
- Heap
- etc.
- Disallow undefined types.
- Fix sourcespan error reporting at object accesses.

Tests
-----

    DOTPROD?
    KMP?
    mapreduce?
    kmeans?



PASTE Demo
----------

+ Substitute and drop trivial preds
    * x: v = x, v >= x  etc.

+ Drop the VV#...

+ PP for function types super wide and gross
    * cf. `forloop` in `minindex`

+ Unify function template names with formal names
    * cf. `forloop` in `minindex`

+ SSA variables? hmm. Perhaps leave
