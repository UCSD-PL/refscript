Benchmarks
----------

  - [DONE] tests/strobe/gadgets/resistor-00.js


JS Features
-----------

  - FIELDS
    - Implement mutability field

  - METHODS:
    - typescript/pos/objects/meth-00.js

  - DYNAMIC WRITES `e1[e2] = e3` 
    - where: `e2` is a `string`

  - CONSTRUCTORS: 

  - CLASSES:

  - INTERFACES:
    - Covariant/Contravariant type parameter positions

  - TRUTHY:
    - Encoding truthy, falsy, undefined, null etc.
      Eg: tc/pos/obj02.js, tc/pos/union05.js

  - Do we need dead code cast?


Tool/Implementation
-------------------
  
  - Fix scoping for functions and variables

  - addInvariant: restrain use

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Null type: only use internally (?)

  - Array literal checks are quite slow.
      E.g.: liquid/pos/arrays/arr-07.js


Failing Tests 
-------------

  TBD

