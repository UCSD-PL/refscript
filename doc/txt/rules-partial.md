
NANO JS
-------

#Language

##Base Types

    B := number                       // Number
       | boolean                      // Boolean
       | string                       // String
       | undefined                    // Undefined
       | null                         // Null
       | void                         // Void
       | top                          // Top
       | A                            // Variables


##Types

    T := {v:B | p}                    // Base types
       | (x1:T1,...,xn:Tn) => T       // Function types
       | forall A. T                  // Quantification
       | T ∪ ... ∪ T                  // Union types
       | {x1:T1,...,xn:Tn}            // Object types       
       | μX.T                         // Recursive types



##Expressions

    E := x                            // Variables
       | c                            // Constants 0, 1, +, -, true...       
       | e1.e2                        // Dot reference
       | {x1:e1,...,xn:en}            // Object literal
       | f[T1,...,Tm](e1,...,en)      // Function Call with type instantiation
       
       | Cast(e,T)                    // Type cast
       
       c.f. @instantiate@ in Language.Nano.Liquid.Liquid.hs


##Constants

    c := 1 | 2 | ...                  // Numeric literals
       | true | false                 // Boolean literals
       | "a" | "foo" | ...            // String literals
       | null                         // Null
       | !                            // Prefix operators
       | + | - | * | /                // Infix operators


##Statements

    S := skip                         // Empty statement 
       | x = e                        // Assignment
       | s1; s2                       // Sequence 
       | if (e) { s1 } else { s2 }    // If statement
       | return e                     // Return


##Functions
    
    F := function f(x1...xn){s}   // Function Definition



##Programs

    P := f1:T1, ... fn:Tn         // Sequence of function definitions


##Simplifying Assumption

`forall` only appears `outside` function types

       forall A B C. (A, B, C) => A

Is ok. But,

       forall A. A

is not ok (no function!) and

       (forall A. A) => int

is not ok (forall appears *inside* =>)





#Environments

- A sequence of type bindings for variables
- No *duplicate* bindings


`G = x1:T1,...,xn:Tn`




##Wellformedness

    G |- p : bool
    ______________
    G |- {v:b | p}

**Intuition**: `p` must be a boolean predicate in `G`



##Embedding Environments

    embed                    :: G -> Predicate
    embed empty              = True                -- Empty Environment
    embed (x1:{v:B | p1}, G) = p1[x1/v] && embed G -- Base Binding
    embed (x1:T, G)          = embed G             -- Non-Base Binding


**Intuition**: Environment is like a Floyd-Hoare **Precondition**


       K2   ∧ K1   => K98
       K912 ∧ K131 => V >= 0
       K912 ∧ K11  => K12 
       K912 ∧ K31  => V >= 0



#On the Fly ANF Conversion

Rejigger typing rules to perform ANF-conversion

    G |- e : G', xe

The typing relation returns:

1. `G'` : the output environment with new temp binders
2. `xe` : the temp binder (in `G'`) corresponding to `e` 



##Example (On the Fly ANF Conversion)

    G |- ((1 + 2) + 3) : G', x'

where 

    G' = G, t0:{v = 1      }
          , t1:{v = 2      }
          , t2:{v = t1 + t2}
          , t3:{v = 3      }
          , t4:{v = t2 + t3}
  
    x' = t4





#Typing


##Expression Typing

Rejigger typing rules to perform ANF-conversion

    G |- e : G', xe

1. `G'` : the output environment with new temp binders
2. `xe` : the temp binder (in `G'`) corresponding to `e` 



###Variables


    ________________[E-Var]

     G |- x : G, x 



###Constants

    z is *FRESH*

    G' = G, z:ty(c)
    _________________[E-Const]

    G |- c : G', z



###Dot reference

    z is *FRESH*

    G |- e1 : G1, x1
    
    S1 = G1(x1)

    unfold(s1) = {f1:T1,...,f:T,...fn:Tn}

    z:Ti

    ______________________[E-DotRef]
    G |- e1.f : G1, z



    z is *FRESH*

    G |- e1 : G1, x1
    
    S1 = G1(x1)

    unfold(s1) = {f1:T1,...,fn:Tn}

    f not in {f1,...,fn} 

    z: undefined

    ______________________[E-DotRef]
    G |- e1.f : G1, z



###Object Literal

    z is *FRESH*

    G |- yi: Ti   foreach i in 1..n

    z:{x1:T1,...,xn:Tn}

    ______________________[E-DotRef]
    G |- {x1:y1,...,xn:yn} : G1, z
 



###(Polymporphic) Function Calls

    z is *FRESH*

    G(f) = forall A1...Am.TBODY 
    
    TBODY[τ1...τm/A1...Am] = (x1:T1...) => T     
    
    G |- yi:Ti'                 foreach i in 1..n
    
    G |- Ti' <: Ti θ            foreach i in 1..n

    Θ = [y1...yn/x1...xn]

    ______________________________________[E-Call]

    G |- f[τ1...τm](y1...yn) : T θ 

Result type is just output type with [actuals/formals]

c.f. @instantiate@ in Language.Nano.Liquid.Liquid.hs



##Statement Typing

    G |- s : G'

G' is G extended with **new bindings** for assigments in `s`



###Statement Typing: skip

    G |- skip : G



###Assign


         G |- e : G',xe
    ________________________

     G |- x = e : G',x:G(xe)


    When x:T in G
    
        G(x) = {v:b| v = x} if T == {v:b|p}
           
               T               otherwise



###Sequence


       G  |- s1 : G1 

       G1 |- s2 : G2
      ___________________[Seq]

      G |- s1; s2 : G2



###Return


    G  |- e : G', xe

    G' |- G'(xe) <: G'($result)
    _____________________________[Ret]

    G  |- return e : Ø



###Branch


    G          |- e : G', xe     
    
    G'(xe)     = {v:boolean | ...}
    
    z is FRESH
    
    G',z:{xe}  |- s1 : G1
    
    G',z:{!xe} |- s2 : G2
    
    G1         |- G1(x) <: T    ∀ x:T in φ 
    
    G2         |- G2(x) <: T    ∀ x:T in φ 
    ____________________________________________

    G |- if [φ] e { s1 } else { s2 } : G+φ      




#Subtyping

    ------
    T <: T 


    ∀i ∃j . Si <: Tj
    --------------------------------
    U Si <: U Tj 

    S <: top

    undefined <: null

    T < T'
    --------------------------------------------  Depth
    { F, s:T; M } < { F, s:T'; M }
     
    { F, s:T; M } < { F; M }          [Width]






##Liquid Subtyping
**Intuition**: Subtyping is like Floyd-Hoare **Rule Of Consequence**
    
    P' => P    {P} c {Q}      Q => Q'
    _________________________________
               {P'} c {Q'}



###Base Types

    b1 <: b2          // Light Subtyping

    (embed G) ∧ K1 => K2
    ________________________________ [Sub-Base]
    G |- {v:b1 | K1} <: {v:b2 | K2}



###Subtyping: Function Types
    
    G,yi:Ti' |- Ti' <: (Ti θ)  foreach i in 1..n
    
    G,yi:Ti' |- T θ <: T'

    θ = [y1..yn/x1..xn]
    ______________________________________________________ [Sub-Fun]

    G |- (x1:T1...xn:Tn) => T <: (y1:T1'...yn:Tn') => T'





#Inference


-----------------------------------------------------------------------------
## What needs to be inferred?

  A: the stuff we DONT want to write down!

    /*@ loop :: ( b:list [int]
                , {min:int | ((0 <= min) && (min < (len b)))}
                , {i:int | 0 <= i})  
                => {v:int | ((0 <= v) && (v < (len b)))} */ 
        
    function loop(b, min, i){
      if (i < length(b)) {
        var min_ = min;
        assert(i < length(b));
        if (nth(b, i) < nth(b, min)) { 
          min_ = i; 
        } 
        return loop(b, min_, i + 1)
      }
      return min;
    }





##Inference Algorithm

###Step 1: Templates

Generate **FRESH** K variables for unknown refinements

    /*@ loop :: ( {b:list [int] | K1}, {min:int | K2}, {i:int | K3}) => {v:int | K4} */ 
    function loop(b, min, i){
      if [min_ :: K8] (i < length[K5](b)) {
        var min_ = min;
        if (nth[K6](b, i) < nth[K7](b, min)) { 
          min_ = i; 
        } 
        return loop(b, min_, i + 1)
      }
      return min;
    }

###Step 2: Perform TypeChecking

Generate a bunch of subtyping requirements

Via subtyping, boils down to subtyping over base types

**Subtyping: Base Types**

       (embed G) ∧ K1 => K98
    _____________________________ [Sub-Base]

    G |- {v:b | K1} <: {v:b | K98}


cf Language.Nano.Liquid.CGMonad.splitC

So **step 2** yields a bunch of **base subtyping constraints**

    x:K2       |- {v:int|K1}      <: {v:int|K9}
    y:K9       |- {v:list int|K3} <: {v:list int| 0 < (len v)} 
    y:K9, b:K1 |- {v:int|K3}      <: {v:int| K2} 
    y:K9, z:K3 |- {v:int|v=y}     <: {v:int| v>=0} 




###Step 3: Solve Constraints via TypeChecking

Subtyping constraints are simply (via definition of **embed**)

        K2[x/v] ∧ K1                 => K9
        K9[y/v] ∧ K3                 => 0 < (len v)
        K9[y/v] ∧ K1[b/v] ∧ K3      => K2 
        K9[y/v] ∧ K3[z/v] ∧ {v = y} => (v >= 0)

Solved via **fixpoint** computation (i.e. abstract interpretation!)



