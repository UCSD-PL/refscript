
/*@ foo :: forall A. (A, A) => A */
function foo(x, y) {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}

/*@ bar :: forall A. (A, B) => A */
function bar(x,y) {
  return foo(x,y);
}
