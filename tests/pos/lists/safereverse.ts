/*@ qualif Len(v:a)               : 0 <= (LLlen v)                 */
/*@ qualif EqLen(v:a, xs:b)       : (LLlen v) = (LLlen xs)            */
/*@ qualif SumLen(v:a, xs:b, ys:c): (LLlen v) = (LLlen xs) + (LLlen ys) */

/*@ reverse :: <A,M extends ReadOnly> (xs: LList<M,A>) => {v: LList<M,A> | (LLlen v) = (LLlen xs)} */
function reverse(xs){

  /*@ go :: (acc: LList<M,A>, ys: LList<M,A>) => {LList<M,A> | (LLlen v) = (LLlen acc) + (LLlen ys)} */ 
  function go(acc, ys){
    if (empty(ys)){
      return acc;
    }
    
    let y    = safehead(ys);
    let ys_  = safetail(ys);
    let acc_ = cons(y, acc);
    
    return go(acc_, ys_);
  }
  let b = nil();
  return go(b, xs);
}
