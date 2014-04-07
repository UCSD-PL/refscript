 /*@ append :: forall A. (xs: #List[A]?, ys: #List[A]?) => {v : #List[A]? | (len v) = (len xs) + (len ys)} */
function append(xs, ys){
  if (empty(xs)) {
    return ys;
  } else {
    var x   = safehead(xs);
    var xs_ = safetail(xs);
    return cons(x, append(xs_, ys));
  }
}


