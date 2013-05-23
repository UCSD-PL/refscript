/*@ forloop :: forall A. (int, int, (int, A) => A, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

/*@ step :: (list [int], int, int) => int */
function step(b, i, min){
  if (nth(b, i) < nth(b, min)) { 
      return i;
  } 
  return min; 
}

/*@ minIndex :: ({a:list [int] | 0 < (len a)}) => {v:int | (0 <= v && v < (len a))} */ 
function minIndex(a){
  return forloop(0, length(a), step, 0);
}
