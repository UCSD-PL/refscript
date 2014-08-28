/*@ forloop :: forall A. (number, number, (number, A) => A, A) => {v:A | true} */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, hi);
  }
  return acc;
}

