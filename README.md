README
=======

Language for experimenting with verification algorithms

nano-js is the basis for the programming assignments in 

    http://goto.ucsd.edu/~rjhala/classes/sp13/cse291

TODO
----

HW 1            <------------------------- HEREHEREHEREHERE

0. Wrap parser

1. Port small tests 
    - int-incr
    - if-max
    - if-abs
    
2. Parse small functions : FilePath -> Com

3. VCGen :: Com -> Pred

4. valid :: Pred -> Bool [liquid-fixpoint wrapper for Pred]

5. Port big tests

    - while-array
    - while-binsearch
    - adpcmini.c

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
