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
/************** Types for list Operators ******************************/
/*************************************************************************/

// ORIGINAL VERSION BEGIN
    /* measure len :: forall A. (list [A]) => number */

    /* cons  :: forall A. (x:A, xs:list [A]) => {v: list [A] | (len v) = 1 + (len xs)}            */
    /* nil   :: forall A. () => {v: list [A] | (len v) = 0}                                       */
    /* head  :: forall A. (xs:list [A]) => A        */
    /* tail  :: forall A. (xs:list [A]) => list [A] */
    /* nth   :: forall A. (xs:list [A], {i:number| ((0 <= i) && i < (len xs))}) => A                 */
    /* empty :: forall A. (xs:list [A]) => {v:boolean | (if (Prop v) then ((len xs) = 0) else ((len xs) > 0))} */
    /* length   :: forall A. (xs:list [A]) => {v:number | ((v >= 0) && v = (len xs))}                */
    /* safehead :: forall A. ({xs:list [A] | (len xs) > 0}) => A                                     */
    /* safetail :: forall A. ({xs:list [A] | (len xs) > 0}) => {v:list [A] | (len v) = (len xs) - 1} */
// ORIGINAL VERSION END


/*@ measure len :: forall A. (list [A]) => number */

/*@ cons  :: forall A. (A, list[A] | Null) => list [A] */
/*@ nil   :: () => Null */
/*@ head  :: forall A. (xs:list [A]) => A        */
/*@ tail  :: forall A. (xs:list [A]) => list [A] */
/*@ nth   :: forall A. (xs:list [A], {i:number| ((0 <= i) && i < (len xs))}) => A                 */
/*@ empty :: forall A. (xs:list [A] | Null) => boolean */
/*@ length   :: forall A. (xs:list [A]) => {v:number | ((v >= 0) && v = (len xs))}                */
/*@ safehead :: forall A. ({xs:list [A] | (len xs) > 0}) => A                                     */
/*@ safetail :: forall A. ({xs:list [A] | (len xs) > 0}) => {v:list [A] | (len v) = (len xs) - 1} */





/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

// ORIGINAL VERSION BEGIN
    /* builtin_OpLT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <  y)) }*/
    /* builtin_OpLEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <= y)) }*/
    /* builtin_OpGT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >  y)) }*/
    /* builtin_OpGEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >= y)) }*/
    //PV: @==@ and @===@ could be handled more precisely
    /* builtin_OpEq        :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x = y)) } */
    /* builtin_OpSEq       :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x = y)) } */
    /* builtin_OpNEq       :: forall A B. ({x:A|true}, {y:B|true}) => {v:boolean | ((Prop v) <=> (x != y)) } */
    /* builtin_OpLAnd      :: ({x:boolean|true}, {y:boolean|true}) => {v:boolean | ((Prop v) <=> ((Prop x) && (Prop y)))} */
    /* builtin_OpLOr       :: ({x:boolean|true}, {y:boolean|true}) => {v:boolean | ((Prop v) <=> ((Prop x) || (Prop y)))}  */
    /* builtin_OpAdd       :: ({x:number | true}, {y:number | true})  => {v:number | v = x + y} */ 
    /* builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v = x - y} */ 
    /* builtin_OpMul       :: (number,  number)  => number                                      */
    /* builtin_OpDiv       :: (number,  number)  => number                                      */ 
    /* builtin_OpMod       :: (number,  number)  => number                                      */
    /* builtin_PrefixMinus :: ({x:number | true}) => {v:number | v = (0 - x)}                */
    /* builtin_PrefixLNot  :: (boolean) => boolean                                     */
    /* builtin_PrefixTypeof:: forall A. (A) => string                                  */
    /* measure prop        :: (boolean) => bool                                        */
// ORIGINAL VERSION END

/*@ builtin_OpLT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <  y)) }*/
/*@ builtin_OpLEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x <= y)) }*/
/*@ builtin_OpGT        :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >  y)) }*/
/*@ builtin_OpGEq       :: ({x:number|true}, {y:number|true}) => {v:boolean | ((Prop v) <=> (x >= y)) }*/
/*@ builtin_OpEq        :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x = y)) } */
/*@ builtin_OpSEq       :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x = y)) } */
/*@ builtin_OpNEq       :: forall A B. ({x:A|true}, {y:B|true}) => {v:boolean | ((Prop v) <=> (x != y)) } */
/*@ builtin_OpLAnd      :: (x:top, y:top) => boolean */
/*@ builtin_OpLOr       :: (x:top, y:top) => boolean */
/*@ builtin_OpAdd       :: ({x:number | true}, {y:number | true})  => {v:number | v = x + y} */ 
/*@ builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v = x - y} */ 
/*@ builtin_OpMul       :: (number,  number)  => number                                      */
/*@ builtin_OpDiv       :: (number,  number)  => number                                      */ 
/*@ builtin_OpMod       :: (number,  number)  => number                                      */
/*@ builtin_PrefixMinus :: ({x:number | true}) => {v:number | v = (0 - x)}                */
/*@ builtin_PrefixLNot  :: (boolean) => boolean                                     */
/*@ builtin_PrefixTypeof:: forall A. (A) => string                                  */
/*@ measure prop        :: (boolean) => bool                                        */






/*************************************************************************/
/************** Pre-Loaded Qualifiers ************************************/
/*************************************************************************/

/*@ qualif Bot(v:a)       : 0 = 1            */
/*@ qualif Bot(v:obj)     : 0 = 1            */ 
/*@ qualif Bot(v:bool)    : 0 = 1            */
/*@ qualif Bot(v:number)     : 0 = 1            */
/*@ qualif CmpZ(v:number)    : v < 0            */    
/*@ qualif CmpZ(v:number)    : v <= 0           */    
/*@ qualif CmpZ(v:number)    : v >  0           */    
/*@ qualif CmpZ(v:number)    : v >= 0           */    
/*@ qualif CmpZ(v:number)    : v =  0           */
/*@ qualif CmpZ(v:number)    : v != 0           */

/*@ qualif Cmp(v:number, x:number)   : v <  x           */    
/*@ qualif Cmp(v:number, x:number)   : v <= x           */    
/*@ qualif Cmp(v:number, x:number)   : v >  x           */    
/*@ qualif Cmp(v:number, x:number)   : v >= x           */    

/*@ qualif Cmp(v:a,x:a)   : v =  x           */    
/*@ qualif Cmp(v:a,x:a)   : v != x           */    
/*@ qualif One(v:number)     : v = 1            */
/*@ qualif True(v:bool)   : (? v)            */
/*@ qualif False(v:bool)  : not (? v)        */
/*@ qualif True1(v:Bool)  : (Prop v)         */                                   
/*@ qualif False1(v:Bool) : not (Prop v)     */




// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*@ qualif Add(v:number,x:number,y:number): v = x + y    */    
/*@ qualif Sub(v:number,x:number,y:number): v = x - y    */    


