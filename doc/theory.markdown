
NANO JS
-------

#Language

##Base Types

    B := number 
       | boolean
       | string
       | undefined
       | null
       | void 
       | top


##Types

    T := {v:B | p}                    // Base types
       | (x1:T1,...,xn:Tn) => T       // Function types
       | {v:A | p}                    // Variables
       | forall A. T                  // Quantification


##Expressions

    E := x                          // Variables
       | c                          // Constants 0, 1, +, -, true...       
       | e1.e2
       | {x1:e1,...,xn:en}
       | Cast(e,T)
       | f[T1,...,Tm](e1,...,en)    // Function Call with type instantiation
       
       c.f. @instantiate@ in Language.Nano.Liquid.Liquid.hs


##Constants

    c := Numeric Literal 
       | Boolean Literal
       | String Literal
       | Null Literal
       | + | - | * | /  


##Statements

    S := skip 
       | x = e
       | s1; s2
       | if (e) { s1 } else { s2 }
       | return e


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






<!--


###Program Typing

    G = f1:T1...fn:Tn
    G |- fi:Ti for i in 1..n
    ___________________________[Program]

      0 |- f1:T1...fn:Tn



###Function Typing

    G, x1:T1...,$result:T |- s  
    ___________________________[Fun]
    G |- function f(x1...){ s } 



###Expression Typing

    G |- e : t
    
In environment `G` the expression `e` evaluates to a value of type `t`

We will see this is problematic, will revisit...




####Constants

    ______________[E-Const]
    G |- c : ty(c) 



**Intuition**: Each constant has a *primitive* or *builtin* type

    ty(1) = {v:int| v = 1}
    
    ty(+) = (x:int, y:int) => {v:int  | v  =  x + y}
    
    ty(<) = (x:int, y:int) => {v:bool | v <=> x < y}

    etc.




**Typing Variables**
   
      G(x) = T 
    _____________[E-Var]

     G |- x : T 



**Typing Function Calls**

    G(f) = (x1:T1...xn:Tn) => T     
    
    G |- ei:Ti'     foreach i in 1..n

    G |- Ti' <: Ti  foreach i in 1..n
    ____________________________________[E-Call]

    G |- f(e1...en) : ??? 

    Uh oh. What type do we give to the *output* of the call?

**Typing Function Calls: What We'd Like**

    + :: (x1:int, x2:int) => {v:int|v = x1 + x2}
    
    +(a,b)              : {v:int | v = a + b}

    Option 1: Direct Substitution
       
    +(foo(a), bar(b))   : {v:int | v = foo(a) + bar(b)}

    - SMT solver knows nothing about `foo` and `bar`
    - Lose all information (e.g. `foo` returns *positive* integers)

    Option 2: Name Intermediate Subexpressions

    var t1 = foo(a)
    var t2 = bar(b) 
    +(t1,t2)

    t1          : {output type of foo, with formal subst. with a}
    t1          : {output type of bar, with formal subst. with b}
    +(t1, t2)   : {v:int | v = foo(t1) + bar(t2)}



**Administrative Normal Form**

a.k.a. **ANF**

Translate program so *every* call is of the form

    f(y1,...,yn)

That is, all arguments are **variables**





**Typing ANF Function Calls**

    G(f) = (x1:T1...) => T     
    
    G |- yi:Ti'         foreach i in 1..n
    
    G |- Ti' <: Ti θ    foreach i in 1..n

    Θ = [y1...yn/x1...xn]
    ______________________________________[E-Call]

    G |- f(y1...yn) : T θ 

    Result type is just output type with [actuals/formals]
-->



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



###Constants

    z is *FRESH*

    G' = G, z:ty(c)
    _________________[E-Const]

    G |- c : G', z



###Variables


    ________________[E-Var]

     G |- x : G, x 



###(Polymporphic) Function Calls
<!--
    G(f) = (x1:T1...) => T     
    
    G   |- e1...en : G', y1...yn

    G'  |- G'(yi) <: Ti   foreach i in 1..n

    θ   = [y1...yn/x1...xn]

    G'' = G', z:T θ     z is *FRESH*
    ______________________________________________[E-Call]

    G   |- f(e1...en) : G'', z  
-->

<!--
**Typing ANF + Polymorphic Function Calls**
-->

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
    
    G1         |- G1(x) <: T    foreach x:T in φ 
    
    G2         |- G2(x) <: T    foreach x:T in φ 
    ____________________________________________

    G |- if [φ] e { s1 } else { s2 } : G+φ      



<!--
###Example 1

    /*@ abs :: ({x:int|true}) => {v:int|v >= 0} */ 
    function abs(x){
      var r = x;
      if (x < 0){
        r = 0 - x;
      } 
      return r;
    }

    /*@ abs :: ({x:int|true}) => {v:int|v >= 0} */ 
    function abs(x){
      /*G0*/ var r0 = x;
      if [r1:{v:int|v>=0}] (x < 0){
        /* G1  */
        r1 = 0 - x;
      } /* G1' */ 
      else {
        /* G2 */
        r1 = r0
      } /* G2' */
      return r1;
    }

    G0  = x:int
    G1  = G0, r0:{v=x},t0:{v:bool| v <=> x < 0}, t0<=>true
    G1' = G1, r1:{v=0-x}
    
    G1' |- G1'(r1) <: {v:int|v >=0}      
    G1' |- {v=r1} <: {v >=0}      

    
r0=x
t0 <=> x < 0
t0<=>true
r1=0-x
v=r1
=> v >= 0       OK!

r0=x
t0 <=> x < 0
t0<=>false
r1=r0
v=r1
=> v >= 0       OK!
 
G0,r1:{v>=0} |- {v=r1} <: {v>=0}



    G2  = G0, r0:{v=x},t0:{v:bool| v <=> x < 0}, t0<=>false
    G2' = G2, r1:{v=r0}
    
    G2' |- G2'(r1) <: {v:int|v>=0}



    /*@ abs :: ({x:int|true}) => {v:int|v >= 0} */ 
    function abs(x){
      var r = x;
      if (x < 0){
        r = 0 - x;
      } 
      return r;
    }


**Example 2**

    /*@ abs :: ((({z:int|z>=0}) => {v>=z})
                
                , int
               ) 
               => {v:int|v >= 0} */ 

    function abs(f, x){
      var r = x;
      if (x < 0){
        r = 0 - x;
      } 
      return f(r);
    }

    /*@ double :: (x:int) => {v:int| v = x + x } */
    function double(x){ return x + x }

    /*@ main :: (int) => {v:int | v>=0} */
    function main(x){
      return /* G */ abs(double, x);
    }



               G |- { x:int | x>=0 }    <: {x:int|true}
               
    G,x:{v:int|v>=0} |- {v:int| v=x+x}  <: {v:int | v>=x}



    G |- {z:int| z>=0}     <: {x:int| true }
    
    G, z:{z:int| z>= 0}  |- {v:int| v=z+z}   <: {v:int| v>=z }
    _____________________________________________________

    G |- ({z:int|true}) => {v:int| v=z+z}
         <: 
         (({z:int|z>=0}) => {v>=z})

        (embed G) && z >= 0 && v = z + z => v >= z
    _____________________________________________________
    
    G, z:{z:int| z>= 0}  |- {v:int| v=z+z}   <: {v:int| v>=z }





**Example 3**
    
    /*@ idt :: forall A. (A) => A */
    function idt(x){ return x}

    /*@ idt :: (T) => T*/
    function idtT(x){ return x}

    /*@ abs :: (x:int) => {v:int | v >= 0} */
    function abs(x){
      var r = x;
      if (x < 0){
        r = 0 - x;
      }
      assert(r >= 0);
      r = idt[T](r);
      return r;
    }

    /*@ main :: (int) => {v:int | v>=0} */
    function main(x){
      var z = abs(double, x);
      assert(z >= 0);
      return id(z);
    }





**Example 3**

    /*@ idt :: forall A. (A) => A */
    function idt(x){ return x}


    /*@ abs :: ({x:int|true}) => {v:int|v >= 0} */ 
    function abs(f, x){
      var r = x;
      if (x < 0){
        r = 0 - x;
      } 
      return idt[{v:int|v>=0}](r);
    }

    /*@ main :: (int) => {v:int | v>=0} */
    function main(x){
      var z = abs(x);
      assert(z >= 0);
      return id(z);
    }

**Lets just focus on the action**

 /*@ abs :: ({x:int|true}) => {v:int|v >= 0} */ 
    function abs(x){
      /*G0*/ var r0 = x;
      if [r1:{v:int|v>=0}] (x < 0){
        /* G1  */
        r1 = 0 - x;
      } /* G1' */ 
      else {
        /* G2 */
        r1 = r0
      } /* G2' */
      /* G3 */
      return idt[{v>=0}](r1);
    }

    G3: x  : true
        r0 : v =  x
        r1 : v >= 0
        {v = r1} <: v>=0  OK



    What are the type instantiation parameters?

        ???


    What are the constraints?

        ???


    All solves out easily.



**Reasoning About Data Structures**

Example: find *index* of smallest element in a list.

         prove that data-structure accesses are SAFE 
         (within bounds)
          
    tests/liquid/pos/minindex01.js

    /*@ loop :: (b:ilist, {min:int | (0 <= min && min < (len b)), 
      {i:int | 0 <= i})   => {v:int | 0 <= v && v < (len b)} */ 
    function loop(b, min, i){
      if (i < length(b)) {
        var min_ = min;
        if (nth(b, i) < nth(b, min)) { 
          min_ = i; 
        } 
        return loop(b, min_, i + 1)
      }
      return min;
    }

    /*@ minIndex :: (list) => int */ 
    function minIndex(a){
      var r = loop(a, 0, 0);
      return r;
    }




**Types?**

    Just think of "int-list" as a type (like int)

    T = {v: ilist | p}


**Properties: Uninterpreted Functions in SMT logic**
    
    len :: (ilist) => int

    So if `a` is a `ilist` of size 10, then

    a   :: {v: ilist | (len v) = 10}


    //LIBRARY
    nth    :: (a:ilist, {i:int | 0 <= i && i < (len a) }) => int
    length :: (a:ilist) => {v:int | v = (len a)}


    



    Give the access "library" function a suitable type

    nth     :: (xs:ilist, {i:int| ((0 <= i) && i < (len xs))}) => int
    
    length  :: (xs:ilist) => {v:int | (v = (len xs))}               

**How to verify properties of data INSIDE containers**

    How shall we prove this assert? [tests/liquid/pos/list00.js]

    /*@ hop :: (list [{v:int | v >= 0}]) => void */
    function hop(xs){
      if (empty[true](xs)) {
        return;
      } else {
        var h = safehead[{v >= 0}](xs);
        assert(0 <= h);    
        hop(safetail[{v>=0}](xs));
      }
    }

    empty     :: forall A. (xs:list [A]) => {v: boolean | ((Prop v) <=> ((len xs) == 0))} 
    safehead  :: forall A. ({v: list [A] | (0 < (len v))) => A
    safetail  :: forall A. ({v: list [A] | (0 < (len v))) => list [A]





**Containers: Types?**

    T := ...
       | {v: C [T1,...,Tn] | r}


**Containers: Expressions?**

**Containers: Wellformedness?**

        G |- Ti     for each i in 1..n

        G, v:C[T1..] |- p : boolean
        ___________________________

        G |- {v: C [T1...Tn] | p}


**Containers: Subtyping?**

        
                  G |- Ti <: Ti'
        
        G, v:C[...] |- p => p' 
        ____________________________________________________

        G |- {v: C [T1...Tn] | p} <: {v: C [T1'...Tn'] | p'}




**Containers: Typechecking?**


-----------------------------------------------------------------------------

-->


#Subtyping


**Intuition**: Subtyping is like Floyd-Hoare **Rule Of Consequence**
    
    P' => P    {P} c {Q}      Q => Q'
    _________________________________
               {P'} c {Q'}




##Base Types

       (embed G) ∧ K1 => K2
    ______________________________ [Sub-Base]
    G |- {v:b | K1} <: {v:b | K2}



##Subtyping: Function Types
    
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



