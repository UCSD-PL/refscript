/*@ crash :: forall A.() => A */
function crash(){
  return crash();
}


/*@ assume :: (bool) => void */ 
function assume(p){
  return;
}

/*@ assert :: (bool) => void */ 
function assert(p){
  return;
}

/*@ requires :: (bool) => void */ 
function requires(p){
  return;
}

/*@ ensures :: (bool) => void */ 
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
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

/*@ builtin_OpLT        :: forall A. (A, A) => bool */
/*@ builtin_OpLEq       :: forall A. (A, A) => bool */
/*@ builtin_OpGT        :: forall A. (A, A) => bool */
/*@ builtin_OpGEq       :: forall A. (A, A) => bool */
/*@ builtin_OpEq        :: forall A. (A, A) => bool */
/*@ builtin_OpNEq       :: forall A. (A, A) => bool */
/*@ builtin_OpLAnd      :: (bool, bool) => bool     */
/*@ builtin_OpLOr       :: (bool, bool) => bool     */
/*@ builtin_OpSub       :: (int,  int)  => int      */ 
/*@ builtin_OpAdd       :: (int,  int)  => int      */
/*@ builtin_OpMul       :: (int,  int)  => int      */
/*@ builtin_OpDiv       :: (int,  int)  => int      */ 
/*@ builtin_OpMod       :: (int,  int)  => int      */
/*@ builtin_PrefixMinus :: (int) => int             */
/*@ builtin_PrefixLNot  :: (bool) => bool           */
