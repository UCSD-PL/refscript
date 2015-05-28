/*@ foo :: forall A B. (A, B) => {v:A | 0 < 1} */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}
