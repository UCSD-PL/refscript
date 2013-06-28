/*@ ranjit :: forall A. (number, number, (number, A) => A, A) => A */
function ranjit(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return ranjit(lo + 1, hi, body, newAcc);
  }
  return acc;
}

/*@ minIndex :: ({a:list [number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a))} */ 
function minIndex(a){
  /*@ step :: (number, number) => number */
  function step(i, min){
    if (nth(a, i) < nth(a, min)) { 
      return i;
    } 
    return min; 
  };
  return ranjit(0, length(a), step, 0);
}
