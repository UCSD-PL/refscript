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

/*@ random :: () => int */
function random(){
  var r = Math.random();
  var x = Math.floor(r * 11);
  return x;
}

/*@ pos    :: () => {v:int | v > 0} */
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

/*@ measure len :: forall A. (list [A]) => int */

/*@ cons  :: forall A. (x:A, xs:list [A]) => {v: list [A] | (len v) = 1 + (len xs)}            */
/*@ nil   :: forall A. () => {v: list [A] | (len v) = 0}                                       */
/*@ head  :: forall A. (xs:list [A]) => A        */
/*@ tail  :: forall A. (xs:list [A]) => list [A] */
/*@ nth   :: forall A. (xs:list [A], {i:int| ((0 <= i) && i < (len xs))}) => A                 */
/*@ empty :: forall A. (xs:list [A]) => {v:boolean | (if (Prop v) then ((len xs) = 0) else ((len xs) > 0))} */
/*@ length   :: forall A. (xs:list [A]) => {v:int | ((v >= 0) && v = (len xs))}                */
/*@ safehead :: forall A. ({xs:list [A] | (len xs) > 0}) => A                                     */
/*@ safetail :: forall A. ({xs:list [A] | (len xs) > 0}) => {v:list [A] | (len v) = (len xs) - 1} */






/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

/*@ builtin_OpLT        :: ({x:int|true}, {y:int|true}) => {v:boolean | ((Prop v) <=> (x <  y)) }*/
/*@ builtin_OpLEq       :: ({x:int|true}, {y:int|true}) => {v:boolean | ((Prop v) <=> (x <= y)) }*/
/*@ builtin_OpGT        :: ({x:int|true}, {y:int|true}) => {v:boolean | ((Prop v) <=> (x >  y)) }*/
/*@ builtin_OpGEq       :: ({x:int|true}, {y:int|true}) => {v:boolean | ((Prop v) <=> (x >= y)) }*/
/*@ builtin_OpEq        :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x = y)) } */
/*@ builtin_OpNEq       :: forall A. ({x:A|true}, {y:A|true}) => {v:boolean | ((Prop v) <=> (x != y)) } */
/*@ builtin_OpLAnd      :: ({x:boolean|true}, {y:boolean|true}) => {v:boolean | ((Prop v) <=> ((Prop x) && (Prop y)))} */
/*@ builtin_OpLOr       :: ({x:boolean|true}, {y:boolean|true}) => {v:boolean | ((Prop v) <=> ((Prop x) || (Prop y)))}  */
/*@ builtin_OpAdd       :: ({x:int | true}, {y:int | true})  => {v:int | v = x + y} */ 
/*@ builtin_OpSub       :: ({x:int | true}, {y:int | true})  => {v:int | v = x - y} */ 
/*@ builtin_OpMul       :: (int,  int)  => int                                      */
/*@ builtin_OpDiv       :: (int,  int)  => int                                      */ 
/*@ builtin_OpMod       :: (int,  int)  => int                                      */
/*@ builtin_PrefixMinus :: ({x:int | true}) => {v:int | v = (0 - x)}                */
/*@ builtin_PrefixLNot  :: (boolean) => boolean                                     */
/*@ measure prop        :: (boolean) => bool                                        */

/*************************************************************************/
/************** Pre-Loaded Qualifiers ************************************/
/*************************************************************************/

/*@ qualif Bot(v:a)       : 0 = 1            */
/*@ qualif Bot(v:obj)     : 0 = 1            */ 
/*@ qualif Bot(v:bool)    : 0 = 1            */
/*@ qualif Bot(v:int)     : 0 = 1            */
/*@ qualif CmpZ(v:int)    : v < 0            */    
/*@ qualif CmpZ(v:int)    : v <= 0           */    
/*@ qualif CmpZ(v:int)    : v >  0           */    
/*@ qualif CmpZ(v:int)    : v >= 0           */    
/*@ qualif CmpZ(v:int)    : v =  0           */
/*@ qualif CmpZ(v:int)    : v != 0           */

/*@ qualif Cmp(v:int, x:int)   : v <  x           */    
/*@ qualif Cmp(v:int, x:int)   : v <= x           */    
/*@ qualif Cmp(v:int, x:int)   : v >  x           */    
/*@ qualif Cmp(v:int, x:int)   : v >= x           */    

/*@ qualif Cmp(v:a,x:a)   : v =  x           */    
/*@ qualif Cmp(v:a,x:a)   : v != x           */    
/*@ qualif One(v:int)     : v = 1            */
/*@ qualif True(v:bool)   : (? v)            */
/*@ qualif False(v:bool)  : not (? v)        */
/*@ qualif True1(v:Bool)  : (Prop v)         */                                   
/*@ qualif False1(v:Bool) : not (Prop v)     */




// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*@ qualif Add(v:int,x:int,y:int): v = x + y    */    
/*@ qualif Sub(v:int,x:int,y:int): v = x - y    */    


