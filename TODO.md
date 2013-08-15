Todo List
=========

0.  Scrape Qualifiers

1.  Fix "Cannot handle ssaVarDECL" at "var foo;"

2.  Encode subtyping rules for types like undefined, null etc. 
    What happens with padding there?
    Eg:
      tc/pos/obj02.js,
      tc/pos/union05.js

3.  Do not add casts deep inside objects.
    Eg:
      tc/pos/listmap02.js 

4.  Comparing objects with null causes upcasts that fail during constraint
    genereation.

5.  Spurious falsified k-vars:
    liquid/pos/minindex02.js


Failing Tests
-------------


