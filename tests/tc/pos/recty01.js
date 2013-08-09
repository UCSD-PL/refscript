/*@ type posInt { number | v>0} */

/*@ type tree[A] { data: A, left: tree[A] + null, right: tree[A] + null } */

/*@ foo :: forall A . (x: list [A]) => void */
function foo(l) {
  return;
}
