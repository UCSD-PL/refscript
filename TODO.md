Todo List
=========

1.  Disallow use of undefined types. I.e. all constructed types should be
    introduced with /*@ type ... */.

2.  Fix sourcespan error reporting at object accesses.

3.  Add an addError variant that ignores (does not print or consider) all error
    that are reported after it.

4.  Fix "Cannot handle ssaVarDECL" at "var foo;"

5.  Add option to not solve constraints

6.  Strings

Strings
-------

+ update TC to support `string` type

+ add a simple test 
  * tests/liquid/pos/str0.js
  * tests/liquid/neg/str0-unsafe.js

- update constraint generation

- run

- Scrape Qualifiers
- unions
- Records
- Objects
- Heap
- etc.
- Disallow undefined types.
- Fix sourcespan error reporting at object accesses.




