
/*@ append :: forall A. (xs:list [A], ys:list [A]) => {v:list [A] | (len v) = (len xs) + (len ys) } */
function append(xs, ys){
  if (empty(xs)) {
    return ys;
  } else {
    var x   = safehead(xs);
    var xs_ = safetail(xs);
    return cons(x, append(xs, ys));
  }
}

 

