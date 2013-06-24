
/*@ id :: forall A . (A) => A */
function id(a) {
  return a;
}

/*@ foo1 :: ((int|boolean|void)) => (boolean|int) */
function foo1(x) {
  id(x);
  return id(x);
}

