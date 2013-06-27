/*@ type List[A]  {  data : A, 
                      next : ( List[A] | Null ) 
                   } */

/*@ foo :: forall A . (x: List[A]) => void */
function foo(l) {
  return;
}
