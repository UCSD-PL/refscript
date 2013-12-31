Benchmarks
----------

  - tests/strobe/gadgets/resistor-01.js


JS Features
-----------

  - FIELDS
    - array "length

  - METHODS:
    - liquid/pos/objects/meth-00.js

  - DYNAMIC WRITES `e1[e2] = e3` 
    - where: `e2` is a `string`

  - CONSTRUCTORS: 
      What should Array(n) return?  In a setting without strong updates, it 
      returns an array of undefined, but this is not very helpful since it will 
      not be able to be updated to anything useful.

  - PROTOTYPES:
    - Add formal description of language, translation and discharge of constraints.

  - TRUTHY:
    - Encoding truthy, falsy, undefined, null etc.
      Eg: tc/pos/obj02.js, tc/pos/union05.js


Tool/Implementation
-------------------

  - Annotation Parser [remove `defs` field from `Nano`]
    - Need to find a way to allow the language-ecmascript parser to parse 
      comments that start with "/*" and end with "*/"

  - Fix "Cannot handle ssaVarDECL" at "var foo;"

  - Restore the check for unbounded/undefined type variables

  - Multiple fixpoint bindings/invariants in the same environment

  - Disallow type to have multiple tags (if possible)

  - Remove duplicate specifications.

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
        /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Do not add casts deep inside objects. Eg: tc/pos/listmap02.js 


Failing Tests 
-------------

[ARRAY.LENGTH]
  
    liquid/pos/arrays/safemap.js

[METHODS]

    liquid/pos/objects/meth-00.js

Failed 57 tests: 
 liquid/pos/arrays/arr-02.js,
 liquid/pos/arrays/arr-03.js,
 liquid/pos/arrays/arr-04.js,
 liquid/pos/arrays/infer-00.js,
 liquid/pos/arrays/safemap.js,
 liquid/pos/lists/list-head-01.js,
 liquid/pos/lists/list-head-02.js,
 liquid/pos/lists/listsum-00.js,
 liquid/pos/lists/listsum-02.js,
 liquid/pos/lists/listsum-03.js,
 liquid/pos/lists/safeLists.js,
 liquid/pos/lists/safehead.js,
 liquid/pos/loops/for-01.js,
 liquid/pos/loops/for-02.js,
 liquid/pos/loops/for-04.js,
 liquid/pos/loops/for-rec.js,
 liquid/pos/loops/obj-00.js,
 liquid/pos/loops/while-01.js,
 liquid/pos/loops/while-02.js,
 liquid/pos/misc/abs-hof.js,
 liquid/pos/misc/abs.js,
 liquid/pos/misc/apply.js,
 liquid/pos/misc/cousot-01.js,
 liquid/pos/misc/cousot.js,
 liquid/pos/misc/dep.js,
 liquid/pos/misc/driver.js,
 liquid/pos/misc/inner-00.js,
 liquid/pos/misc/locks-cond.js,
 liquid/pos/misc/locks.js,
 liquid/pos/misc/max.js,
 liquid/pos/misc/minindex-00.js,
 liquid/pos/misc/minindex-01.js,
 liquid/pos/misc/minindex-02.js,
 liquid/pos/misc/poly-ctx-00.js,
 liquid/pos/misc/twice-hof.js,
 liquid/pos/misc/twice.js,
 liquid/pos/objects/infer-00.js,
 liquid/pos/objects/infer-01.js,
 liquid/pos/objects/infer-03.js,
 liquid/pos/objects/meth-00.js,
 liquid/pos/objects/obj-01.js,
 liquid/pos/objects/obj-03.js,
 liquid/pos/operators/add-02.js,
 liquid/pos/operators/add-04.js,
 liquid/pos/operators/add-06.js,
 liquid/pos/operators/id-00.js,
 liquid/pos/operators/id-01.js,
 liquid/pos/operators/id-02.js,
 liquid/pos/operators/inc-00.js,
 liquid/pos/operators/inc-01.js,
 liquid/pos/operators/stmt.js,
 liquid/pos/operators/sum-00.js,
 liquid/pos/operators/sum-01.js,
 liquid/pos/operators/sum-02.js,
 liquid/pos/operators/sum-infer-00.js,
 liquid/pos/operators/sum-infer-01.js,
 liquid/pos/simple/abs-00.js

Exceptions thrown on 57 tests:
 liquid/pos/arrays/arr-02.js,
 liquid/pos/arrays/arr-03.js,
 liquid/pos/arrays/arr-04.js,
 liquid/pos/arrays/infer-00.js,
 liquid/pos/arrays/safemap.js,
 liquid/pos/lists/list-head-01.js,
 liquid/pos/lists/list-head-02.js,
 liquid/pos/lists/listsum-00.js,
 liquid/pos/lists/listsum-02.js,
 liquid/pos/lists/listsum-03.js,
 liquid/pos/lists/safeLists.js,
 liquid/pos/lists/safehead.js,
 liquid/pos/loops/for-01.js,
 liquid/pos/loops/for-02.js,
 liquid/pos/loops/for-04.js,
 liquid/pos/loops/for-rec.js,
 liquid/pos/loops/obj-00.js,
 liquid/pos/loops/while-01.js,
 liquid/pos/loops/while-02.js,
 liquid/pos/misc/abs-hof.js,
 liquid/pos/misc/abs.js,
 liquid/pos/misc/apply.js,
 liquid/pos/misc/cousot-01.js,
 liquid/pos/misc/cousot.js,
 liquid/pos/misc/dep.js,
 liquid/pos/misc/driver.js,
 liquid/pos/misc/inner-00.js,
 liquid/pos/misc/locks-cond.js,
 liquid/pos/misc/locks.js,
 liquid/pos/misc/max.js,
 liquid/pos/misc/minindex-00.js,
 liquid/pos/misc/minindex-01.js,
 liquid/pos/misc/minindex-02.js,
 liquid/pos/misc/poly-ctx-00.js,
 liquid/pos/misc/twice-hof.js,
 liquid/pos/misc/twice.js,
 liquid/pos/objects/infer-00.js,
 liquid/pos/objects/infer-01.js,
 liquid/pos/objects/infer-03.js,
 liquid/pos/objects/meth-00.js,
 liquid/pos/objects/obj-01.js,
 liquid/pos/objects/obj-03.js,
 liquid/pos/operators/add-02.js,
 liquid/pos/operators/add-04.js,
 liquid/pos/operators/add-06.js,
 liquid/pos/operators/id-00.js,
 liquid/pos/operators/id-01.js,
 liquid/pos/operators/id-02.js,
 liquid/pos/operators/inc-00.js,
 liquid/pos/operators/inc-01.js,
 liquid/pos/operators/stmt.js,
 liquid/pos/operators/sum-00.js,
 liquid/pos/operators/sum-01.js,
 liquid/pos/operators/sum-02.js,
 liquid/pos/operators/sum-infer-00.js,
 liquid/pos/operators/sum-infer-01.js,
 liquid/pos/simple/abs-00.js

