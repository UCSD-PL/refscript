JS SUPPORT / FEATURES:

  - FIELDS
    - array "length
     
  - METHODS:
    - liquid/pos/objects/meth-00.js
     
  - DYNAMIC WRITES `e1[e2] = e3` 
    - where: `e2` is a `string`
    
  - CONSTRUCTORS: 
      What should Array(n) return?  In a setting without strong updates, it 
      returns an array of undefined, but this is not very helpful since it will 
      not be able to be updated to anything useful.
  
  - PROTOTYPES:
    - Add formal description of language, translation and discharge of constraints.
  
  - TRUTHY:
    - Encoding truthy, falsy, undefined, null etc.
      Eg: tc/pos/obj02.js, tc/pos/union05.js


TOOL/IMPLEMENTATION:

  - Type/predicate aliases
  
  - Annotation Parser [remove `defs` field from `Nano`]
    - Need to find a way to allow the language-ecmascript parser to parse 
      comments that start with "/*" and end with "*/"
  
  - Fix "Cannot handle ssaVarDECL" at "var foo;"
  
  - Restore the check for unbounded/undefined type variables
  
  - Multiple fixpoint bindings/invariants in the same environment
  
  - Disallow type to have multiple tags (if possible)
  
  - Remove duplicate specifications.

  - Fix hacky qualifier parse -> translation e.g. tests/liquid/pos/arrays/arr-03.js
        /*@ qualif OkLen(v:number, arr:a): v < (len arr) */
        Note use of lower-case which gets translated into tyvars in fixpoint. sigh.
      
  ? Do not add casts deep inside objects. Eg: tc/pos/listmap02.js 


Failing Tests 
-------------

[ARRAY.LENGTH]
  
    liquid/pos/arrays/safemap.js
