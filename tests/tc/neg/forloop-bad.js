/*@ forloop :: forall A. (number, number, (number, A) => B, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

