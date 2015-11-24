
/*@ foo :: forall A. (A, A) => {v:A |0 < 1} */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}

/*@ bar :: forall A. (A, B) => {v:A | 0 < 1} */
function bar(x,y) {
  return foo(x,y);
}
