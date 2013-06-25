
/*@ id :: forall A . (A) => A */
function id(a) {
  return a;
}

/*@ foo1 :: ((int|boolean|void)) => (boolean|int) */
function foo1(x) {

  var a = 1;
  a = id(x);
  return a;
}

