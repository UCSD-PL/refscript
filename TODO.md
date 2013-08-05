Todo List
=========

0.  Scrape Qualifiers

1.  Disallow use of undefined types. I.e. all constructed types should be
    introduced with /*@ type ... */.

2.  Fix sourcespan error reporting at object accesses.

3.  Add an addError variant that ignores (does not print or consider) all error
    that are reported after it.

4.  Fix "Cannot handle ssaVarDECL" at "var foo;"

5.  Add option to not solve constraints

Failing Tests
-------------

Failed 33 tests: 
liquid/neg/abs-join-01.js,
 liquid/neg/list02-bad.js,
 liquid/neg/listsum-bad.js,
 liquid/neg/minindex02-bug.js,
 liquid/neg/safeList-bad.js,
 liquid/neg/safeappend-bad.js,
 liquid/pos/abs-hof.js,
 liquid/pos/apply.js,
 liquid/pos/cousot.js,
 liquid/pos/driver.js,
 liquid/pos/forloop.js,
 liquid/pos/inc.js,
 liquid/pos/inc0.js,
 liquid/pos/list00.js,
 liquid/pos/list02.js,
 liquid/pos/listsum.js,
 liquid/pos/listsum00.js,
 liquid/pos/locks-cond.js,
 liquid/pos/locks.js,
 liquid/pos/map.js,
 liquid/pos/minindex00.js,
 liquid/pos/minindex01.js,
 liquid/pos/minindex02.js,
 liquid/pos/safeLists.js,
 liquid/pos/safeappend.js,
 liquid/pos/safemap.js,
 liquid/pos/safereverse.js,
 liquid/pos/stmt.js,
 liquid/pos/sum-infer-no-bug.js,
 liquid/pos/sum-infer.js,
 liquid/pos/sum-return.js,
 liquid/pos/sum-ssa.js,
 liquid/pos/twice-hof.js

Exceptions thrown on 16 tests:
 liquid/neg/abs-join-01.js,
 liquid/neg/list02-bad.js,
 liquid/neg/listsum-bad.js,
 liquid/neg/minindex02-bug.js,
 liquid/neg/safeList-bad.js,
 liquid/neg/safeappend-bad.js,
 liquid/pos/list00.js,
 liquid/pos/list02.js,
 liquid/pos/listsum.js,
 liquid/pos/listsum00.js,
 liquid/pos/map.js,
 liquid/pos/minindex02.js,
 liquid/pos/safeLists.js,
 liquid/pos/safeappend.js,
 liquid/pos/safemap.js,
 liquid/pos/safereverse.js


 run

- Scrape Qualifiers
- unions
- Records
- Objects
- Heap
- etc.
- Disallow undefined types.
- Fix sourcespan error reporting at object accesses.




