/////////////////////////////////////////////////////////////////
// Paste Demo 3//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/*@ range :: (int, int) => list[int] */
function range(lo, hi) {
  if (lo < hi) {
    res = range(lo + 1, hi);
    res = push(lo, res);
    return res;
  }
  return empty();
}

/*@ reduce :: forall A B. (list[B], A, (A, B) => A) => A */
function reduce(xs, acc, f){
  if (!isEmpty(xs)) {
    acc = f(acc, top(xs));
    acc = reduce(pop(xs), acc, f);
  }
  return acc;
}

/* minIndex :: ({a:list [int] | true}) => int */            // BAD SPEC 
/*@ minIndex :: ({a:list [int] | 0 < (len a)}) => int */    // GOOD SPEC

function minIndex(a){
  /*@ step :: (int, int) => int */
  function step(min, i){
    if (a[i] < a[min]) 
      min = i;
    return min;
  }
  var is = range(0, length(a));
  return reduce(is,0,step);
}

