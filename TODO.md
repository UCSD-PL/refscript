Todo List
=========

0.  Scrape Qualifiers

1.  Prefix -- boolean negation: fix refinements

2.  Fix "Cannot handle ssaVarDECL" at "var foo;"

3.  Encode subtyping rules for types like undefined, null etc. 
    What happens with padding there?
    Eg:
      tc/pos/obj02.js,
      tc/pos/union05.js

4.  Do not add casts deep inside objects.
    Eg:
      tc/pos/listmap02.js 

5.  Comparing objects with null causes upcasts that fail during constraint
    genereation.

6.  Spurious falsified k-vars:
    liquid/pos/minindex02.js

7.  Need to restore the check for unbounded type variables.

8.  Multiple fixpoint bindings in the same environments. 

9.  For/while loops

01. Prototyping


Failing Tests
-------------


