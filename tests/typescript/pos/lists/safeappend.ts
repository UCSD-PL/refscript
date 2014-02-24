 /*@ append :: forall A. (xs:list[A] + null, ys:list[A] + null) => {v : list[A] + null | (len v) = (len xs) + (len ys)} */
function append(xs, ys){
  if (empty(xs)) {
    return ys;
  } else {
    var x   = safehead(xs);
    var xs_ = safetail(xs);
    return cons(x, append(xs_, ys));
  }
}


