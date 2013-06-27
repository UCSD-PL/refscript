/*@ type List[A] = {  data : A, 
                      next : ( List[A] | Null ) 
                   } */

/*@ forall A . (x: List[A]) => void */
function foo(l) {
  return;
}
