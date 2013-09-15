/*@ foo :: ( x: [number] + null ) => { number | true } */
function foo (x) {
  if (!emptyPoly(x)) {
    return x[1];
  }
  return 0;

}
