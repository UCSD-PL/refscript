Tests / Benchmarks
------------------

### Failing tests

    neg/misc/abs-join-00.ts,              // TS translation exception
    pos/arrays/arr-11.ts,                 
    pos/loops/obj-01.ts,
    pos/loops/obj-02.ts,
    pos/misc/animals.ts,                  // numbers of arguments
    pos/misc/negate-00.ts,                // tag information
    pos/misc/negate-05.ts,                // intersection types - TC under false condition
    pos/misc/negate-06.ts,                // <same>
    pos/objects/obj-03.ts,                // unsupported bracket-ref
    pos/simple/ambient-00.ts,             // annotation missing - fixed modules
    pos/simple/ambient-01.ts,             // fixed in modules
    pos/unions/union-04.ts                // unions

### Exceptions 
    
    neg/misc/abs-join-00.ts,
    pos/simple/ambient-01.ts


### Working benchmarks
  
    tests/strobe/gadgets/resistor-00.js


JS Features
-----------


Tool/Implementation
-------------------
  
  - What are good default mutabilities (parsing etc.)?

  - Tidy up output files

  - Revisit mutability subtyping, compute variance of type parameter in type

  - TApp (TRef ...) ... is now prefixed with a '#' to disambiguate from TVar.

  - Do co/contra-variant checks depending on mutability.

  - Enforce invariant that overloaded functions signatures are non-overlapping

  - Checks on type parameters (including mutability - always first parameter)
  
  - Encode method mutability
  
  - Variables cannot be named: "func" or "obj" (fixpoint restriction)

  - Test examples with infinite loops.

  - Add sanity check for types: each sort should be represented at most once at
    a union type top-level.

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Array literal checks are quite slow.
      E.g.: typescript/pos/arrays/arr-07.js


TS benchmarks
-------------

    underscore    885     641 
    d3            9,225   1,558 
    ace           13,483  615 
    fabricjs      8,584   740 
    jquery        6,043   526
    pixi          5,829   361 
    box2dweb      10,718  1,139 
    leaflet       5,873   707 
    threejs       19,065  2,651 
    sugar         4,133   650

