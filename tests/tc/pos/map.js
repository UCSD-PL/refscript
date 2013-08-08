/*@ map :: forall A B. ((A) => B, list [A] + null) => list[B] + null */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
       
  var y   = f(xs.data);
  
  var ys = map(f, xs.next);

  return cons(y, ys);

}

