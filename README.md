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
1. Port small tests [int-incr, if-abs]
2. Parse small functions : FilePath -> Statement
3. vcgen :: Statement -> [Pred]                     
4. valid :: Pred -> IO Bool     -- liquid-fixpoint wrapper for Pred

5. Port big tests [tests/flat/pos/] <------------ HEREHEREHERE JUST GET THESE TO PARSE

6. Add functions
   
    - Nano = [Function a]

    - function foo(args){
        requires(p);
        ensures(p);
        BODY;
      }

    - Output value called: "$result" named resultVar in (Types.hs)

    - type FunName = String

    - data FunSpec = FSpec { fname :: FunName 
                           , fargs :: [String] (or var?)
                           , fpre  :: Pred
                           , fpost :: Pred
                           }

    - type Env     = M.Map FunName FunSpec

    - makeEnv      :: Nano -> FunSpec
    - vcgen        :: FunSpec -> Nano -> [(a, Pred)]

    - OLD vcgen 
   
    HEREHEREHEREHERE

    + FunSpec in the VCMonad so you have access to spec
    + CALL   = assert (f-pre); assume (f-post)
    + ENTRY  = assume (f-pre)
    + RETURN = assert (f-post) ; assume (false)

7. Release
    - mkdir assignment
    - delete all lines "invariant"
    - delete vcgen-statement implementation
    - write  README.md

8. Add arrays


HINTS 
-----

Since the VCGen happens using a monad to log "side-conditions",
you may find the `<=<` operator quite handy.

For example, 

to generate the VC for a sequence of commands 

    c1;c2;c3

that is to compute

    generateVC (c1; c2; c3) vc 

you can do something like

    (generateVC c1 <=< generateVC c2 <=< generateVC c3) vc


Make sure you understand:

    `Language.Fixpoint.Types.Subable`

You will need to implement substitutions, as needed for x := e, etc.

    `Language.Fixpoint.Types.Symbolic`

    http://goto.ucsd.edu/~rjhala/llvm-haskell/doc/html/liquidtypes/Language-Haskell-Liquid-Fixpoint.html#t:Subable

You may need this to convert program variables `Id a` to logical symbols `F.Symbol`

    `Language.Fixpoint.Types.Expression`

You may need this to convert program expressions `Expression a` to logical expressions `F.Expr`

    `Language.Fixpoint.Types.Predicate`

You may need this to convert program expressions `Expression a` to logical predicates `F.Pred`

(For the last three, the relevant class instances are in `Language.Nano.Types`)




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
