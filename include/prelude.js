/*@ crash :: forall A.() => A */
function crash(){
  return crash();
}


/*@ assume :: (boolean) => void */ 
function assume(p){
  return;
}

/*@ assert :: ({x:boolean|(Prop x)}) => void */ 
function assert(p){
  return;
}

/*@ requires :: (boolean) => void */ 
function requires(p){
  return;
}

/*@ ensures :: (boolean) => void */ 
function ensures(p){
  return;
}

/*@ random :: () => number */
function random(){
  var r = Math.random();
  var x = Math.floor(r * 11);
  return x;
}

/*@ pos    :: () => {v:number | v > 0} */
function pos(){
  var x = random();
  if (x > 0) {
    return x;
  } else {
    return (1 - x);
  }
}



/*************************************************************************/
/*********************** Temporary tag maps ******************************/
/*************************************************************************/
// From:
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
//
// Undefined                                                              "undefined"
// Null                                                                   "object"
// Boolean                                                                "boolean"
// Number                                                                 "number"
// String                                                                 "string"
// Host object (provided by the JS environment)  Implementation-dependent
// Function object (implements [[Call]] in ECMA-262 terms)                "function"
// E4X XML object                                                         "xml"
// E4X XMLList object                                                     "xml"
// Any other object                                                       "object"
//
// For Null
// typeof null === 'object'; // This stands since the beginning of JavaScript



/*************************************************************************/
/***************** Types for list Operators ******************************/
/*************************************************************************/

/*@ type list[A]  {  data : A, next : list[A] + null } */

/*@ measure len :: forall A. (list [A]) => number                                                 */

/*@ cons  :: forall A. (A, xs:list[A] + null) => {list[A] | (len v) = 1 + (len xs)}               */
/*@ nil   :: () => { null | (len v) = 0}                                                           */
/*@ head  :: forall A. (xs:list[A]) => A                                                         */
/*@ tail  :: forall A. (xs:list [A]) => list [A] + null                                           */
/*@ nth   :: forall A. (xs:list [A], {i:number| ((0 <= i) && i < (len xs))}) => A                 */
/*@ empty :: forall A. (xango: list[A] + null ) => 
                        {v: boolean | (((Prop v) <=> len(xango) = 0) && ((Prop v) <=> ttag(xango) = "null"))}                      */
/*@ emptyPoly :: forall A. (x:A) => {v: boolean | ((Prop v) <=> ((ttag x) = "null"))}             */

/*@ length   :: forall A. (xs:list[A] + null) => {v:number | ((v >= 0) && v = (len xs))}         */
/*@ safehead :: forall A. (list[A]) => A                                     */
/*@ safetail :: forall A. (xs:list[A]) => {v:list[A] + null | (len v) = (len xs) - 1} */

/*@ Array    :: (n : { v: number | 0 <= v } ) => { v: [ undefined ] | (len v) = n }               */



/*************************************************************************/
/************************* Type Conversions ******************************/
/*************************************************************************/
/*@ sstring  :: forall A. (x: A) => string                               */





/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

/*@ builtin_BIBracketRef     :: forall A. (arr:[A], {idx:number | (0 <= idx && idx < (len arr))}) => A           */
/*@ builtin_BIBracketAssign  :: forall A. (arr:[A], {idx:number | (0 <= idx && idx < (len arr))}, val:A) => void */
/*@ builtin_BIArrayLit       :: forall A. (A) => {v:[A] | (len v) = builtin_BINumArgs}                           */

/*@ builtin_BIUndefined     :: forall A. {A | false} */




/*@ builtin_OpLT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <  y)) }   */
/*@ builtin_OpLEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <= y)) }   */
/*@ builtin_OpGT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >  y)) }   */
/*@ builtin_OpGEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >= y)) }   */

//PV: @==@ and @===@ could be handled more precisely
/*@ builtin_OpEq        :: forall A.   (x:A, y:A) => {v:boolean | ((Prop v) <=> (x = y)) }  */
/*@ builtin_OpSEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x = y)) }  */
/*@ builtin_OpNEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) } */

/*@ builtin_OpLAnd      :: ({x:top|true}, {y:top|true}) => {v:boolean | true}                             */
/*  builtin_OpLAnd      :: ({x:top|true}, {y:top|true}) =>
                            {v:boolean | ((Prop v) <=> (if (falsy x) then (v = y) else (v = x)))}         */

/*@ builtin_OpLOr       :: ({x:boolean|true}, {y:boolean|true}) =>
                            {v:boolean | ((Prop v) <=> ((Prop x) || (Prop y)))}                           */

// XXX: Will eventually switch to truthy and falsy:
/*  builtin_OpLOr       :: (x:top, y:top) => 
      { top | ((Prop v) <=> (if (falsy x) then (v = y) else (v = x) ))}           */


//TODO: We would like to have a more precise type (like the following) that 
//would include strings into the game, but this does not work well with 
//equality at the moment:

/*@ builtin_OpAdd :: /\ (x:number, y:number) => {number | v = x + y}
                     /\ (x:number, y:string) => string
                     /\ (x:string, y:number) => string
                     /\ (x:string, y:string) => string
  */

/* builtin_OpAdd      :: (x:number + string, y:number + string) => 
                              {v:number + string | (if ((ttag x) = "number" && (ttag y) = "number") 
                                                    then ((ttag v) = "number" && v = x + y) 
                                                    else (ttag v)  = "string")} 
  */

/* builtin_OpAdd       :: ({x:number | true}, {y:number | true})  => {v:number | v = x + y}              */

// The following causes problems with equality. Can't "prove" 0 == (0 + 1) as
// the RHS has type {num | v = 0 + 1} + string. The crucial fact is buried under
// the sum -- the `top-level` refinement is `top` which is useless.

/* builtin_OpAdd       :: (x:number + string, y:number + string) => 
                              {number | ((ttag x) = "number" && (ttag y) = "number" && v = x + y) } + 
                              {string | ((ttag x) = "string" || (ttag y) = "string") } 
                      
  */


/*@ builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v = x - y}              */
/*@ builtin_OpMul       :: (number,  number)  => number                                                   */
/*@ builtin_OpDiv       :: (number,  number)  => number                                                   */
/*@ builtin_OpMod       :: (number,  number)  => number                                                   */
/*@ builtin_PrefixMinus :: ({x:number  | true}) => {v:number  | v = (0 - x)}                              */
/*@ builtin_PrefixLNot  :: ({x:boolean | true}) => {v:boolean | ((Prop v) <=> not (Prop x))}              */

//Changing temprorarily until strings are supported
/* builtin_PrefixTypeof:: forall A. (A) => string                                                         */


/*@ measure prop        :: (boolean) => bool                              */

/*************************************************************************/
/************** Run-Time Tags ********************************************/
/*************************************************************************/

/*@ measure ttag :: forall A. (A) => string                               */

/*@ builtin_PrefixTypeof:: forall A. (x:A) => {v:string | (ttag x) = v }  */

/*@ invariant {v:undefined | ttag(v) = "undefined"} */
/*@ invariant {v:null      | ttag(v) = "null"     } */  //TODO: this is not very precise
/*@ invariant {v:boolean   | ttag(v) = "boolean"  } */ 
/*@ invariant {v:number    | ttag(v) = "number"   } */
/*@ invariant {v:string    | ttag(v) = "string"   } */
/*@ invariant {v:object    | ttag(v) = "object"   } */


/*************************************************************************/
/************** Pre-Loaded Qualifiers ************************************/
/*************************************************************************/

/*@ qualif Bot(v:a)       : 0 = 1                               */
/*@ qualif Bot(v:obj)     : 0 = 1                               */
/*@ qualif Bot(v:bool)    : 0 = 1                               */
/*@ qualif Bot(v:number)     : 0 = 1                            */
/*@ qualif CmpZ(v:number)    : v < 0                            */
/*@ qualif CmpZ(v:number)    : v <= 0                           */
/*@ qualif CmpZ(v:number)    : v >  0                           */
/*@ qualif CmpZ(v:number)    : v >= 0                           */
/*@ qualif CmpZ(v:number)    : v =  0                           */
/*@ qualif CmpZ(v:number)    : v != 0                           */

/*@ qualif Cmp(v:number, x:number)   : v <  x                   */
/*@ qualif Cmp(v:number, x:number)   : v <= x                   */
/*@ qualif Cmp(v:number, x:number)   : v >  x                   */
/*@ qualif Cmp(v:number, x:number)   : v >= x                   */

/*@ qualif Cmp(v:a,x:a)   : v =  x                              */
/*@ qualif Cmp(v:a,x:a)   : v != x                              */
/*@ qualif One(v:number)     : v = 1                            */
/*@ qualif True(v:bool)   : (? v)                               */
/*@ qualif False(v:bool)  : not (? v)                           */
/*@ qualif True1(v:Bool)  : (Prop v)                            */
/*@ qualif False1(v:Bool) : not (Prop v)                        */




// Somewhat more controversial qualifiers (i.e. "expensive"...)

/* qualif Add(v:number,x:number,y:number): v = x + y           */
/* qualif Sub(v:number,x:number,y:number): v = x - y           */


/*@ top_level :: () => void */
