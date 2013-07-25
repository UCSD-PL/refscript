
/*@ foo0 :: ((number|boolean)) => (boolean|number) */
function foo0(x) {
  return x;
}

/*@ foo1 :: ((number|boolean)) => (boolean|number) */
function foo1(x) {
  return x;
}

/*@ foo2 :: ((number|boolean)) => (boolean|number|void) */
function foo2(x) {
  return x;
}



/*@ foo4 :: forall A. (A) => A */
function foo4(x) {
  return x;
}


