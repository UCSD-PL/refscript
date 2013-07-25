/*@ forloop :: forall A. (number, number, (number, A) => A, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(acc, lo);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

