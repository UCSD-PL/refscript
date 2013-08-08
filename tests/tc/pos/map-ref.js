
/*@ map :: forall A B. (
                        (A) => B, 
                        { v: list [A] | not (null v) } + {v:null | (null v)} )
                      => { v: list[B] | not (null v) } + {v:null | (null v)} */
function map(f, xs) {
  if (empty(xs)) {
    return nil();
  }
       
  var y   = f(xs.data);
  
  var ys = map(f, xs.next);

  return cons(y, ys);

}

