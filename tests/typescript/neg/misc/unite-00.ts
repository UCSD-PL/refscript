/*@ foo :: forall A B. (A, B) => A */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}
