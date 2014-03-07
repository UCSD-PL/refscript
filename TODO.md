Benchmarks
----------

  - [DONE] tests/strobe/gadgets/resistor-00.js


JS Features
-----------

  - FIELDS
    - definitely missing field

  - METHODS:
    - liquid/pos/objects/meth-00.js

  - DYNAMIC WRITES `e1[e2] = e3` 
    - where: `e2` is a `string`

  - CONSTRUCTORS: 
      What should Array(n) return?  In a setting without strong updates, it 
      returns an array of undefined, but this is not very helpful since it will 
      not be able to be updated to anything useful.

  - CLASSES:
      Nominal or structural subtyping?
      var a /*@ A */ = new A();

      var a /*@ { ... the actual fields of A or less ... } */ = new A();

  - PROTOTYPES:
    - Add formal description of language, translation and discharge of constraints.

  - TRUTHY:
    - Encoding truthy, falsy, undefined, null etc.
      Eg: tc/pos/obj02.js, tc/pos/union05.js


Tool/Implementation
-------------------
  
  - Optionally print all the inferred types.

  - Add resolved class types to the defined data type environment (tDefs).

  - Pay attention to capital first letter at function/class defs.

  - Restore the check for unbounded/undefined type variables

  - Multiple fixpoint bindings/invariants in the same environment

  - Disallow type to have multiple tags (if possible)

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
        /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - There is a confusion with reserved type names (e.g. "null") and defined
    type (any identifier can be considered as a defined type). So it's very easy
    to confuse "null" with "Null". So disallow all types that are not defined
    properly (i.e. through alias, type etc.)

  - Array literal checks are quite slow.
      E.g.: liquid/pos/arrays/arr-07.js

  - Restore the (liquid) property of Array.length (i.e. that it returns a number
    equal to the length of the array)

Failing Tests 
-------------

[ARRAY.LENGTH]
  liquid/pos/lists/safeList.js

[METHODS]
  liquid/pos/objects/meth-00.js

[REGEXP-PARSE]
  liquid/pos/objects/obj-02-parse-bug.js,

[K-VAR INSTANTIATION]
 liquid/pos/lists/safeLists.js,
 liquid/pos/misc/abs-00.js,

[PROTOTYPES - NOT DONE]
 liquid/pos/proto/proto-lookup3.js,
 liquid/pos/proto/proto-lookup4.js,
 liquid/pos/proto/proto-lookup5.js,
 liquid/pos/proto/proto-lookup6.js

