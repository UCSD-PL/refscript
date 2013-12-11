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

RJ TODO
-------
    + fix ARR-READ                    <----------------------- HEREHEREHEREHERE 
    + fix ARR-WRITE (arrays/arr-04.js)
    + fix OBJ
    + fix RIGID
    + add support for predicate aliases
    + scrape qualifiers
    + type and predicate aliases


Failing Tests 
-------------

Exceptions thrown on 33 tests:

 [ARR]
 liquid/pos/arrays/arr-00.js,
 liquid/pos/arrays/arr-01.js,
 liquid/pos/arrays/arr-02.js,
 liquid/pos/arrays/arr-03.js,
 liquid/pos/arrays/arr-04.js,
 liquid/pos/arrays/arr-05.js,
 liquid/pos/arrays/safemap.js,

 [OBJ]
 liquid/pos/lists/list-01.js,
 liquid/pos/lists/list-02.js,
 liquid/pos/lists/list-03.js,
 liquid/pos/lists/list-head-01.js,
 liquid/pos/lists/list-head-02.js,
 liquid/pos/loops/obj-00.js,
 liquid/pos/loops/obj-01.js,
 liquid/pos/loops/obj-02.js,
 liquid/pos/objects/obj-00.js,
 liquid/pos/objects/obj-01.js,
 liquid/pos/objects/obj-02.js,
 liquid/pos/objects/obj-03.js,
 liquid/pos/objects/obj-04.js,
 liquid/pos/objects/obj-05.js,
 liquid/pos/objects/obj-subtype-00.js,
 liquid/pos/objects/obj-subtype-01.js,
 liquid/pos/objects/update-00.js,
 liquid/pos/objects/update-01.js,
 liquid/pos/objects/update-02.js,
 liquid/pos/objects/update-03.js,
 liquid/pos/simple/obj-00.js,
 liquid/pos/simple/parse-01.js


 [LOOPINV+OBJ]
 liquid/pos/loops/while-04.js,
 
 [RIGID]
 liquid/pos/misc/apply.js,
 liquid/pos/misc/cousot-01.js,
 liquid/pos/misc/twice-hof.js,

# vim:ft=quicktask
