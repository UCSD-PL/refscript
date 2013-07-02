/*@ type list[A]  {  data : A, 
                     next : list[A] | Null } */

/*@ map :: forall A B. ((A) => B, list [A]|Null) => list[B] | Null */
function map(f, xs){
  
  if (empty(xs)) {
    return nil();
  }
  
  var y   = f(xs.data);
  
  var ys_ = map(f, xs.next);

  return cons(y, ys_);

}

