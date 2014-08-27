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

  - METHODS:
    - typescript/pos/objects/meth-00.js


Varargs
-------

Failed 16 tests: 
 neg/misc/locks-bad.ts,


Tool/Implementation
-------------------
  
  - Parsing exceptions are not captured by regrtest.py

  - Do co/contra-variant checks depending on mutability.

  - Enforce invariant that overloaded functions signatures are non-overlapping

  - Add check for number of parameters passed to generic type
  
  - Encode method mutability
  
  - Check that the first argument of a generic type is in { ReadOnly, Immutable,
    AssignFields, Mutable }.

  - Arguments cannot be named: "func"

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

