function minIndex(a){
  requires(length(a) > 0);
  ensures(0 <= $result && $result < (length(a)))
  function loop(min, i){
    var minNext = min;
    if (i < length(a)) {
      if (a[i] < a[min]) { minNext = i; } 
      return loop(min_new, i+1)
    }
    return min;
  }
  return loop(0, 0);
}


