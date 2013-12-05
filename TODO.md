# Quicktask v1.2
# http://quicktask.aaronbieber.com/doc.html

JS SUPPORT / FEATURES:
  - Dynamic Writes `x[i] = e`
    @ Added [Mon 2013-11-24]
  - Type/predicate aliases
    @ Added [Mon 2013-11-24]
  - For/While loops
    @ Added [Sun 2013-09-15]
    @ Start [Sun 2013-09-15] [15:59]
    @ DONE [Sun 2013-10-20]
  - Arrays:
    - Array access: add array bounds checks or return undefined. Right now an
      array access is unsound in that it returns the contained type regardless
      of the access index
      @ Added [Sun 2013-09-15]
      @ Start [Sun 2013-09-15] [15:53]
    - What should Array(n) return?  In a setting without strong updates, it 
      returns an array of undefined, but this is not very helpful since it will 
      not be able to be updated to anything useful.
      @ Added [Sun 2013-09-15]
    @ Added [Sun 2013-09-15]
  - Prototyping
    @ Added [Sun 2013-09-15]
  - Add formal description of language, translation and discharge of 
    constraints.
    @ Added [Fri 2013-10-18]
  - Encoding truthy, falsy, undefined, null etc.
    Eg: tc/pos/obj02.js, tc/pos/union05.js
    @ Added [Sun 2013-09-15]


TOOL / IMPLEMENTATION:
  - Scrape Qualifiers
    @ Added [Sun 2013-09-15]
  - Annotation Parser
    - Need to find a way to allow the language-ecmascript parser to parse 
      comments that start with "/*" and end with "*/"
    @ Added [Mon 2013-10-28]
    @ DONE [Mon 2013-11-04]
  - Fix "Cannot handle ssaVarDECL" at "var foo;"
    @ Added [Sun 2013-09-15]
  - Do not add casts deep inside objects.
    Eg: tc/pos/listmap02.js 
    @ Added [Sun 2013-09-15]
  - Restore the check for unbounded/undefined type variables
    @ Added [Sun 2013-09-15]
  - Multiple fixpoint bindings/invariants in the same environment
    @ Added [Sun 2013-09-15]
  - Disallow type to have multiple tags (if possible)
    @ Added [Sun 2013-09-15]
  - Remove duplicate invariants.
    @ Added [Sun 2013-09-15]


FAILING TESTS:
  - Is "liquid/neg/minindex02-bug.js - line 5" indeed a bug?
    Using new representation of lists.
    @ Added [Sun 2013-09-15]

Failed 16 tests: 
 liquid/pos/arrays/arr-04.js,
 liquid/pos/arrays/safemap.js,
 liquid/pos/lists/safeLists.js,
 liquid/pos/lists/safemap.js,
 liquid/pos/lists/safereverse.js,
 liquid/pos/loops/obj-02.js,
 liquid/pos/loops/while-03.js,
 liquid/pos/misc/apply.js,
 liquid/pos/misc/twice-hof.js,
 liquid/pos/objects/obj-03.js,
 liquid/pos/operators/sum-00.js,
 liquid/pos/operators/sum-01.js,
 liquid/pos/operators/sum-02.js,
 liquid/pos/operators/sum-infer-00.js,
 liquid/pos/simple/obj-00.js

Exceptions thrown on 25 tests:
 liquid/pos/arrays/arr-04.js,
 liquid/pos/arrays/safemap.js,
    liquid/pos/lists/listsum-00.js,
 liquid/pos/lists/safeLists.js,
 liquid/pos/lists/safemap.js,
 liquid/pos/lists/safereverse.js,
    liquid/pos/loops/for-01.js,
    liquid/pos/loops/for-02.js,
    liquid/pos/loops/for-03.js,
    liquid/pos/loops/for-04.js,
    liquid/pos/loops/obj-00.js,
    liquid/pos/loops/obj-01.js,
 liquid/pos/loops/obj-02.js,
    liquid/pos/loops/while-01.js,
 liquid/pos/loops/while-03.js,
 liquid/pos/misc/apply.js,
    liquid/pos/misc/cousot.js,
 liquid/pos/misc/twice-hof.js,
 liquid/pos/objects/obj-03.js,
    liquid/pos/operators/inc-00.js,
    liquid/pos/operators/inc-02.js,
    liquid/pos/operators/sum-01.js,
    liquid/pos/operators/sum-02.js,
    liquid/pos/operators/sum-infer-00.js,
 liquid/pos/simple/obj-00.js



RJ TODO
-------
    + add support for string +    (liquid/pos/operators/add-0{3,4,5}.js)
    + intersections
    + add support for array-write (liquid/pos/arrays/arr-04.js)
    + add support for predicate aliases

Intersections
-------------

Allow intersection types

    * limited to top level intersections for functions
    * no type parameters
    * will be ordered

    + test [test/liquid/pos/operators/add-07.js]
    + RType
    + Parser
    + Visitors

    4. tcFun
    5. tcCall
    6. consFun
    7. consCall
    8. splitC


Scrape Qualifiers
-----------------


Predicate and Type Aliases
--------------------------



# vim:ft=quicktask
