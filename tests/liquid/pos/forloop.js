/*@ forloop :: forall A. (number, number, (number, A) => A, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

/*@ plus :: (number, number) => number */
function plus(i, x) { return x + i ; }

/*@ main :: ({n:number| n > 0}) => void */
function main(n){
  var m = forloop(0, n, plus, n);
  assert(m >= n);
}
