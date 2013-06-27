
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


/*@ foo3 :: forall A. (A) => (A|int) */
function foo3(x) {
  var a = 1;
  if (a > 10) 
    return a;
  return x;
}


/*@ foo4 :: forall A. (A) => A */
function foo4(x) {
  return x;
}


