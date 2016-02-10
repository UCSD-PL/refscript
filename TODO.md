
Tool/Implementation
-------------------

  - Validity check for function types: 
    
    * Optional arguments should only occupy the end part of a type.

    * Refinements should not reference optional arguments.

  - Better error message for `errorCallNotSup`.

  - Get rid of syb

  - Implement Sort checker for refinements
  
  - Add script compilation in stack build script

  - fixEnums has been disabled -- handle at typechecking

  - Disallow (buggy): module K.L.M, which can be replaced by

      module K { module L ... }

  - Mutability

      * Check method invocation for mutability

      * Checks on type parameters (including mutability - always first parameter)

  - Variables cannot be named: "func" or "obj" (fixpoint restriction)

  - WELL-FORMEDNESS CHECKS:

    * TRefs should have a mutability position

    * each sort should be represented at most once at a union

    * overloaded functions signatures are non-overlapping

  - Fix hacky qualifier parse-translation e.g. tests/liquid/pos/arrays/arr-03.js

          /* qualif OkLen(v:number, arr:a): v < (len arr) */

    Note use of lower-case which gets translated into tyvars in fixpoint. sigh.

  - Check polarity of type parameter in type

