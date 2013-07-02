
/*@ type list[A]  {  data : A, 
                     next : list[A]  } */

/*@ fold :: (list[A], (A,B) => B, B) => B */
function fold(l,f,b) {


  return fold(l.next, f, f(l.data,b));

}
