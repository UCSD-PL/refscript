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


