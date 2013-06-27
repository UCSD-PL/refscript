
/*@ foo0 :: ((int|bool)) => (bool|int) */
function foo0(x) {
  return x;
}

/*@ foo1 :: ((int|bool)) => (bool|int) */
function foo1(x) {
  return x;
}

/*@ foo2 :: ((int|bool)) => (bool|int|void) */
function foo2(x) {
  return x;
}



/*@ foo4 :: forall A. (A) => A */
function foo4(x) {
  return x;
}


