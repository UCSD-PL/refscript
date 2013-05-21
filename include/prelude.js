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

// pos    :: () -> {v:Int | v > 0}

/*@ pos    :: () => int */
function pos(){
  var x = random();
  if (x > 0) {
    return x;
  } else {
    return (1 - x);
  }
}

/*************************************************************************/
/************** Builtin Qualifiers ***************************************/
/*************************************************************************/

/*@ qualif Bot(v:int): 0 = 1 */
/*@ qualif NonNeg(v:int): v >= 0 */

/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

/*@ builtin_OpLT        :: (int, int) => boolean          */
/*@ builtin_OpLEq       :: (int, int) => boolean          */
/*@ builtin_OpGT        :: (int, int) => boolean          */
/*@ builtin_OpGEq       :: (int, int) => boolean          */
/*@ builtin_OpEq        :: forall A. (A, A) => boolean    */
/*@ builtin_OpNEq       :: forall A. (A, A) => boolean    */
/*@ builtin_OpLAnd      :: (boolean, boolean) => boolean  */
/*@ builtin_OpLOr       :: (boolean, boolean) => boolean  */
/*@ builtin_OpSub       :: (int,  int)  => int            */ 
/*@ builtin_OpAdd       :: (int,  int)  => int            */
/*@ builtin_OpMul       :: (int,  int)  => int            */
/*@ builtin_OpDiv       :: (int,  int)  => int            */ 
/*@ builtin_OpMod       :: (int,  int)  => int            */
/*@ builtin_PrefixMinus :: (int) => int                   */
/*@ builtin_PrefixLNot  :: (boolean) => boolean           */

/*@ measure prop        :: (boolean) => bool              */

