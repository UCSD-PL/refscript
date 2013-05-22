/*@ forloop :: forall A. (int, int, (int, A) => A, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

/*@ plus :: (int, int) => int */
function plus(i, x) { return x - i ; }

/*@ main :: ({n:int| n > 0}) => void */
function main(n){
  var m = forloop(0, n, plus, n);
  assert(m >= n);
}
