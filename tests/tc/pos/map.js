/*@ type list[A]  {  data : A, 
                     next : list[A] | Null } */

// #define list?[A] = {list[A] | ...} | {Null | ... }

/*@ map :: forall A B. ((A) => B, list [A]|Null) => list[B]|Null */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
       
  var y   = f(xs.data);
  
  var ys = map(f, xs.next);

  return cons(y, ys);

}

