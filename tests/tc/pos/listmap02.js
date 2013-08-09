/*@ type list_non_null[A]  {  data : A, 
                     next : list[A]  } */

/*@ map :: forall A B. (list_non_null[A], (A) => B) => list_non_null[B] */
function map(x,f) {

  return { 
    data: f(x.data) , 
    next: map(x.next, f)
  };

}
