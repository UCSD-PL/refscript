Todo List
=========

JS support / Features
---------------------

0.  What should Array(n) return? 
    In a setting without strong updates, it returns an array of undefined, but
    this is not very helpful since it will not be able to be updated to anything
    useful.

1.  prelude.js: restore refinemnets in logical negation

2.  Encoding truthy, falsy, undefined, null etc.
    Eg: tc/pos/obj02.js, tc/pos/union05.js

3.  For/while loops

4.  Prototyping


Tool / Implementation
---------------------
1.  Scrape Qualifiers

2.  Fix "Cannot handle ssaVarDECL" at "var foo;"

3.  Do not add casts deep inside objects.
    Eg: tc/pos/listmap02.js 

4.  Restore the check for unbounded type variables

5.  Multiple fixpoint bindings in the same environment

6.  Disallow type to have multiple tags


Failing Tests
-------------
1.  Is "liquid/neg/minindex02-bug.js - line 5" indeed a bug?
    Using new representation of lists.

