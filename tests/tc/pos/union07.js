
/*@ id :: forall A . (A) => A */
function id(a) {
  return a;
}

/*@ foo1 :: ((number|boolean|void)) => (boolean|number) */
function foo1(x) {

  var a = 1;
  a = id(x);
  return a;
}

