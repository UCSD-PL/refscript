/* reverse :: forall A. (list [A]) => list [A] */

/*@ reverse :: forall A. (xs:list [A]) => {v:list [A] | (len v) = (len xs)} */

function reverse(xs){

  //HIDE REFTYPE
  /* go :: (list [A], list[A]) => list [A] */ 
  
  /*@ go :: (acc:list [A], ys: list[A]) => {v: list [A] | (len v) = (len acc) + (len ys)} */ 
  function go(acc, ys){
    if (empty(ys)){
      return acc;
    }
    var y    = safehead(ys);
    var ys_  = safetail(ys);
    var acc_ = cons(y, acc);
    return go(acc_, ys_);
  }
  return go(nil(), xs);
}

