
JS SUPPORT / FEATURES:
  - Dynamic Writes `x[i] = e`
  - Type/predicate aliases
  - For/While loops
  - Arrays:
    - Array access: add array bounds checks or return undefined. Right now an
      array access is unsound in that it returns the contained type regardless
      of the access index
    - What should Array(n) return?  In a setting without strong updates, it 
      returns an array of undefined, but this is not very helpful since it will 
      not be able to be updated to anything useful.
  - Prototyping
  - Add formal description of language, translation and discharge of 
    constraints.
  - Encoding truthy, falsy, undefined, null etc.
    Eg: tc/pos/obj02.js, tc/pos/union05.js


TOOL / IMPLEMENTATION:
  - Scrape Qualifiers
  - Annotation Parser
    - Need to find a way to allow the language-ecmascript parser to parse 
      comments that start with "/*" and end with "*/"
  - Fix "Cannot handle ssaVarDECL" at "var foo;"
  - Do not add casts deep inside objects.
    Eg: tc/pos/listmap02.js 
  - Restore the check for unbounded/undefined type variables
  - Multiple fixpoint bindings/invariants in the same environment
  - Disallow type to have multiple tags (if possible)
  - Remove duplicate invariants.


FAILING TESTS:
  - Is "liquid/neg/minindex02-bug.js - line 5" indeed a bug?
    Using new representation of lists.

RJ TODO
-------

    + type and predicate aliases      <------------------------ HEREHEREHEREHEREHERE
    
    + fix LENGTH
    
    + fix hacky qualifier parse -> translation e.g. tests/liquid/pos/arrays/arr-03.js
        /*@ qualif OkLen(v:number, arr:a): v < (len arr) */
        Note use of lower-case which gets translated into tyvars in fixpoint. sigh.
      
Failing Tests 
-------------

[ARRAY.LENGTH]
  
    liquid/pos/arrays/safemap.js,


# vim:ft=quicktask
