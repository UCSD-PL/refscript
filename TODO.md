Tests / Benchmarks
------------------

See https://github.com/UCSD-PL/rs-benchmarks

  

Tool/Implementation
-------------------

  - Disallow (buggy): module K.L.M, which can be replaced by 

      module K { module L ... }


  - Mutability 

      * What are good default mutabilities (parsing etc.)?

      * Checks on type parameters (including mutability - always first parameter)
  

  - Variables cannot be named: "func" or "obj" (fixpoint restriction)


  - WELL-FORMEDNESS CHECKS: 
      
    * each sort should be represented at most once at a union

    * overloaded functions signatures are non-overlapping


  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js
        
          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.


  - Check polarity of type parameter in type


  - Array literal checks are quite slow.
      E.g.: typescript/pos/arrays/arr-07.js


