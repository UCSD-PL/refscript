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
  
  - Check that the first argument of a generic type is in { ReadOnly, Immutable,
    AssignFields, Mutable }.

  - Check number of arguments at generic instantiations.

  - Fix scoping for functions and variables.

  - Add check for types of extended classes.

  - Arguments cannot be named: "func"

  - Test examples with infinite loops.

  - Add sanity check for types: each sort should be represented at most once at
    a union type top-level.

  - Add check for missing type definition

  - Merge pp' with pp - should be the same now that Object types are TCons.

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Array literal checks are quite slow.
      E.g.: typescript/pos/arrays/arr-07.js


Failing Tests 
-------------

  TBD

