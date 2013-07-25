

/////////////////////////////////////////////////////////////////
// Paste Demo 2//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/*@ loop :: forall A. (int, int, A, (int, A) => A) => A */
function loop(lo, hi, acc, body){
  if (lo < hi) {
    acc = body(lo, acc);
    return loop(lo + 1, hi, acc, body);
  }
  return acc;
}

/* minIndex :: ({a:list [int] | true}) => int */            // BAD SPEC 
/*@ minIndex :: ({a:list [int] | 0 < (len a)}) => int */    // GOOD SPEC
function minIndex(a){
  /*@ step :: (int, int) => int */
  function step(i, min){
    if (a[i] < a[min]) 
      min = i;
    return min;
  }
  return loop(0, length(a), 0, step);
}


