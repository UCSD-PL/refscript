
#LANGUAGE DESCRIPTION



##1. Core Language
  
  * Where we stand in terms of JS and TS

  * Core parts of JavaScript
    - Imperative, object oriented
    - Array, object literals
    - First class functions
    - Prototyping (through class inheritance)
    - Dynamic fields (using the TS object map approach)
    - Runtime tag checking

  * Subset of TypeScript
    - Static typing
    - Class based prototyping/inheritance
    - Generics
    - Scoping



##2. Representation / Scoping

  * Locals variables: 
    - SSA
    - Allow strong updates

  * Globals vars: 
    - Annotatated
    - Infer invariant refinements
    - No strong update

  * Block based scoping



##3. Base type system 

  * Comparison to some of TypeScript's type system highlights
    - Signarure overloading / intersection types
    - Excluding dynamic type (any)
    - Avoiding some "known" type holes (covariant function argument subtyping)

  * Union types (untagged)

  * Named types:
    - Classes, inferfaces (inheriance)

  * Parametric Polymorphism

  * Structural types - deep interplay with named types

  * Deep structural unification and subtyping:
    - modulo recursive types



##4. Refinement type system (Liquid types)

  * Language: equality, arithmetic, uninterpreted predicates
  
  * Buit on top of base type system
    - Can be applied to any level of a type tree

  * Subtyping
    - Boils down to logical implications
    - Possible only if base types are equal

    - Challenge: subtyping on the base type system level
    - Using cast mechanism to achieve this

  * What do we gain by using SSA?



##5. Cast mechanism

  * Implicit downcasts
    - Only applicable with union types

  * Implicit upcasts
    - Used to equate base types to enable refinement type checking
    - Addressing the challenge mentioned in (4.)

  * The `||` operator
    - Matching up equivalent parts of two input type trees
    - Structural equality of the two operands
    - Semanntic equivalence with input types by using appropriate refinements



##6. Control flow

  * Path sensitive constraint solving
    - Gather conditionals as guard and used to strengthen constraint premises

  * Loops
    - Infer invariants on phi-vars
    - Generate constraints based on initial variables and loop condition
    - Use liquid type engine to solve for them

    - Example 




# APPLICATIONS

##1. Mutability checking / sound array bounds checks
  
  * IGJ style mutability modifiers:
  
    - Support for all "object" types (values with `typeof(v) === "object"`)
    - Retrofiting from classes and generics

  * Simple example using arrays:
  
    - Combining overloading and mutability
    - Inferring the right kind of mutability
    - Soundly checking array bounds

  * Perhaps a more complicated example
  
    - Involving uniqueness and mutability
    - *unimplemented*

  * Benchmark: 
    - TSC: `compiler/core/arrayUtils.ts`



##2. Sound overload checking

  * TypeScript does not check overloaded signatures

    - Example:
      ```
      createElement(tagName: "div"): HTMLDivElement; 
      createElement(tagName: "span"): HTMLSpanElement; 
      createElement(tagName: "canvas"): HTMLCanvasElement; 
      createElement(tagName: string): HTMLElement;          // Actually checked by TS 
      ```

    - Use of precise refinements to capture string constant types



##3. Sound treatment of downcasts

  * TypeScript allows unsound downcasts:
    - Part of them could be discharged soundly based on tag/instanceof
      information
    - Example:
        ```
        if (ast.kind() === VariableDecl) {
          vdecl = <VariableDecl>cur;
        }
        ```



##4. Use of unions instead of the top type:

  * Example from TS compiler:
      ```
      private _textOrWidth: any;

      public width(): number { return typeof this._textOrWidth === 'number' ?
        this._textOrWidth : this._textOrWidth.length; }

      public text(): string {
        if (typeof this._textOrWidth === 'number') {
            this._textOrWidth = "some string..."; [...]
        }
        return this._textOrWidth;
      }
      ```


###5. JQuery (perhaps)

  *Don't think I have the story for that yet*
