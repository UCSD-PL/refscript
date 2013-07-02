/*@ type list[A]  {  data : A, 
                     next : list[A]  } */

/*@ map :: (list[A], (A) => B) => list[B] */
function map(x,f) {

  return { 
    data: f(x.data) , 
    next: map(x.next, f)
  };

}
