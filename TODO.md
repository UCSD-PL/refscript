Tests / Benchmarks
------------------

### Working benchmarks
  
    tests/strobe/gadgets/resistor-00.js


Tool/Implementation
-------------------

  - Disallow (buggy): module K.L.M, which can be replaced by 
      module K { module L ... }

  - Enforce invariant that overloaded functions signatures are non-overlapping

  - Infer variable argument signatures from TS sigs with '?' arguments.

  - Mutability 

      * What are good default mutabilities (parsing etc.)? Mutability annotations
        are very ugly (see pos/objects/obj-08.ts).

      * Checks on type parameters (including mutability - always first parameter)
  
  - Variables cannot be named: "func" or "obj" (fixpoint restriction)

  - Add sanity check for types: each sort should be represented at most once at
    a union type top-level.

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Check polarity of type parameter in type

  - Array literal checks are quite slow.
      E.g.: typescript/pos/arrays/arr-07.js


Rewrites
--------

while (i <- f(i) ; cond(i)) {
  BODY
}

===>

i = f(i);
while (cond(i)) {
  BODY
  i = f(i);
}

Ex:

while (++i < n) {
  STUFF
}

===>

i = i + 1;
while (i < n){
  STUFF
  i = i + 1;
}
