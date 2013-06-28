
/*@ foo0 :: ((number|bool)) => (bool|number) */
function foo0(x) {
  return x;
}

/*@ foo1 :: ((number|bool)) => (bool|number) */
function foo1(x) {
  return x;
}

/*@ foo2 :: ((number|bool)) => (bool|number|void) */
function foo2(x) {
  return x;
}



/*@ foo4 :: forall A. (A) => A */
function foo4(x) {
  return x;
}


