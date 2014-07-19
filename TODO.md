Benchmarks
----------

  - [DONE] tests/strobe/gadgets/resistor-00.js


JS Features
-----------

  - METHODS:
    - typescript/pos/objects/meth-00.js

  - Do we need dead code cast?


Tool/Implementation
-------------------
  
  - Do co/contra-variant checks depending on mutability.

  - Enforce invariant that overloaded functions have signatures with
    non-overlapping signatures.

  - Add check for number of parameters passed to generic type

  - Add single place (perhaps in parser) where all input types are checked.

  - Add mutability in tsc

  - Multiple constructors

  - Type aliases as top-level types
  
  - Encode method mutability
  
  - Check that the first argument of a generic type is in { ReadOnly, Immutable,
    AssignFields, Mutable }.

  - Check number of arguments at generic instantiations.

  - Arguments cannot be named: "func"

  - Test examples with infinite loops.

  - Add sanity check for types: each sort should be represented at most once at
    a union type top-level.

  - Add check for missing type definition

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
