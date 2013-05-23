/*@ qualif UBound(v:int, x:a) : v < (len x) */

/*@ loop :: (list [int], int, int) => int */ 
function loop(b, min, i){
  if (i < length(b)) {
    var min_ = min;
    assert(i < length(b));
    if (nth(b, i) < nth(b, min)) { 
      min_ = i; 
    } 
    return loop(b, min_, i + 1)
  }
  return min;
}

/*@ minIndex :: ({a:list [int] | 0 < (len a)}) => {v:int | (0 <= v && v < (len a))} */ 
function minIndex(a){
  var r = loop(a, 0, 0);
  return r;
}


