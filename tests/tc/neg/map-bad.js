/*@ map :: forall A B. ((A) => B, list [A]) => list [B] */
function map(f, xs){
  
  if (empty(xs)) {
    return nil();
  }
  
  // return cons(f(head(xs)), map(f, tail(xs)));

  var x0  = head(xs);
  var xs_ = tail(xs);
  var y   = x0;
  var ys_ = map(f, xs_);
  return cons(y, ys_);

}

