/*@ type posInt { number | v>0} */

/*@ type list[A] { data: A, next: list[A] | null }  */

/*@ type tree[A] { data: A, left: tree[A] | null, right: tree[A] | null } */

/*@ foo :: forall A . (x: list [A]) => void */
function foo(l) {
  return;
}
