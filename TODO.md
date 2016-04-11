TOP PRIORITY

- Disallow use of `len v` for mutable arrays.

-  Check variance of type parameters

- W-F checks:
    * overloaded functions signatures are non-overlapping

- Validity check for function types:
    * Optional arguments should only occupy the end part of a type.
    * Refinements should not reference optional arguments.

- Proper bidirectional checking at TC

- SSA: Patching the annotations is probably not necessary - they can be dirrecly
       applied to the nodes as the SSAed AST is being built.

       Perhaps the same with TC phase.

- Better error message for the ill-formed annotation:

    /*@ readonly a */
    let a = 1;

--------------------------------------------------------------------------------

- Better error message for `errorCallNotSup`.

- Get rid of syb

- Implement Sort checker for refinements

- fixEnums has been disabled -- handle at typechecking
