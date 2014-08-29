/*@ foo :: forall A B. (A, B) => {v:A | true} */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}
