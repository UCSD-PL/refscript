Tests / Benchmarks
------------------

### Failing tests



### Working benchmarks
  
    tests/strobe/gadgets/resistor-00.js


Tool/Implementation
-------------------

  - instanceOf refinement: only for class constructed types - not interfaces 

  - Infer TS method types

  - TApp (TRef ...) ... is now prefixed with a '#' to disambiguate from TVar.

  - Enforce invariant that overloaded functions signatures are non-overlapping

  - Mutability 

      * What are good default mutabilities (parsing etc.)? Mutability annotations
        are very ugly (see pos/objects/obj-08.ts).

      * Checks on type parameters (including mutability - always first parameter)

      * Revisit mutability subtyping, compute variance of type parameter in type
        Do co/contra-variant checks depending on mutability.
  
      * Encode method mutability
  
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

