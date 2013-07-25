/*@ type list[A]  {  data : A, 
                     next : list[A] | Null } */

/*@ foo :: forall A . (x:list[A]) => list[int] */
function foo(l) {
  return l;
}
