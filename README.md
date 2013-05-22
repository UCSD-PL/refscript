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

Failed 10 tests: 
    
    liquid/pos/inc.js,      {--------------- HEREHEREHEREHEREHEREHERE
    
    AARGH. When instantiating:

       forall A. ({x:A|true},{y:A|true}) -> ...

    with 
        
        A = [int]

    we are screwed because 
    
        fresh :: [int] --> [{vRANDOM:int| K1}] 

    and now "meeting this with the "x:A|true" obliterates the "x" from the sig... 
    
    YIKES.

    meaning the subsequent SUBSTITUtION IS NOT OVER x but the vRANDOM...

    
    liquid/pos/driver.js, 
    liquid/pos/locks-fun.js, 
    liquid/pos/sum-return.js, 
    liquid/pos/sum.js, 
    liquid/pos/while5.js


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

---------------------------------------------------------------------------------------



Base Types
----------

B := Int

Records
-------

N := \[a] -> { x:T.. }

Types
-----

S := \[A..].T

T := B
   | a
   | (Ti..) -> T
   | N [T..]

    TyVars(T) \subseteq \cup_i TyVars(Ti)
    -------------------------------------
              |- (xi:Ti..) -> T


Expressions
-----------

e := x                          -- var
   | c                          -- int const
   | e `o` e                    -- int op
   | e `r` e                    -- int rel
   | e `b` e                    -- bool op 
   | e (e..)                    -- Function Call 
   | {x:e..}                    -- Object Literal
   | x.f                        -- Field Read
   | function f(xi..){c} :: S

Commands
--------

c := x = e          -- assignment
   | c; c           -- sequence
   | if e c1 c2     -- if-then-else
   | while e c      -- while-loop [NUKE]
   | return e       -- return
   | x.f = e        -- x := {x with f = e}


Programs
--------

p := (f::S)..


Typing Environments
-------------------

G := 0 | G, x:T 


Typing Rules 
------------

**Expressions** [G |- e => t]

---------------
G |- x => (G x)

-------------
G |- c => Int

G |- e1 => Int G |- e2 => Int
-----------------------------
G |- e1 `o` e2 => Int

G |- e1 => Int G |- e2 => Int
-----------------------------
G |- e1 `r` e2 => Bool

G |- e1 => Bool G |- e2 => Bool 
-------------------------------
G |- e1 `b` e2 => Bool


N = \[A..] -> {fi:Ti...}    
G |- ei => Ti'  
unify(Ti.., Ti'..) = θ 
--------------------------------
G |- {fi:ei} => N [0 A]


G(x) = N [T..]   F(N) = \[A..] -> {f:Tf}
----------------------------------------
G |- x.f => Tf[T../A..]

G |- e  => \[A].(Ti) -> T    
G |- ei => Ti'  
unify(Ti.., Ti'..) = θ 
-----------------------------------
G |- e (ei..) => θ T 

**Commands** [G |- c => G' + EXIT]

G |- e => T
--------------------
G |- x = e => G, x:T

G |- c1 => EXIT
-----------------------
G |- c1; c2 => EXIT 

G |- c1 => G1       G1 not EXIT      G1 |- c2 => G2
---------------------------------------------------
G |- c1; c2 => G2

G |- e  => Bool     G |- c1 => G1   G |- c2 => G2
--------------------------------------------------
G |- if e c1 c2 => G1 `join` G2

G |- e => Bool      G |- c => G'
---------------------------------
G |- while e c => G `join` G'

G |- e => T   T = G($result) 
----------------------------
G |- return e => EXIT

G(x) = N [T..]      F(N) = \[A..] -> {f:Tf}     G |- e => Tf[T../A..]
----------------------------------------------------------------------
G |- (x.f = e) => G 

**Functions** [G |- function f...]

S  = forall a...(Ti..) -> T
G' = G, a.., xi:Ti.., $result:T
G' |- c => EXIT
-----------------------------------
G |- function f(xi..){c} :: S => S 

**Programs** [ |- (f::S).. ]

G = (fi::Si)..    foreach i. G |- fi::Si : Si 
----------------------------------------------
|- (fi::Si)..

where 

join G1 G2             = [x:T | x:T in G1 and x:T in G2] 
join EXIT(_) G         = G
join G EXIT(_)         = G
join (EXIT T) (EXIT T) = Bot T

---------------------------------------------------------------------

How to modify for Refinement Types?

0. [SKIP WHILE]

1. Explicit JOIN for types (uses MEET for CONTRA)

2. CHECK Subtyping at function call? [CHECK]

3. CHECK Subtyping at return? [CHECK]

1. [NO] Mark certain assignments as SSA
   Induce Subtyping Constraints there

2. LOOP CHECK? [NO WHILE?]

