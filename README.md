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


TODO
----

HW 1            

0. Wrap parser

1. Port small tests 
    - int-incr
    - if-max
    - if-abs
    
2. Parse small functions : FilePath -> Statement

3. vcgen :: Statement -> [Pred]                     <------------------------- HEREHEREHEREHERE

4. valid :: Pred -> IO Bool     -- liquid-fixpoint wrapper for Pred

5. Port big tests

    - while-array
    - while-binsearch
    - adpcmini.c

6. Add functions


Homework Plan
-------------

HW 1
1a. VCG 
1b. Use ESC/J

HW 2
2a. ConsGen = VCG+K for LoopInv via FIXPOINT
2b. Implement FIXPOINT (over liquid-fixpoint)

HW 3
3a. VCG for Refinement Type Checking
3b. Consgen = VCG+K for Liquid Type Inference via FIXPOINT
