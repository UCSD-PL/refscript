
/*@ foo :: forall A. (A, A) => {v:A |true} */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}

/*@ bar :: forall A. (A, B) => {v:A | true} */
function bar(x,y) {
  return foo(x,y);
}
