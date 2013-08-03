/*@ type list[A]  {  data : A, 
                     next : list[A] + Null } */

/*@ foo :: forall A . (x:list[A]) => list[A] */
function foo(l) {
  return l;
}
