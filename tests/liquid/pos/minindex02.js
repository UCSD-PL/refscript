function range(lo, hi) {
  if (lo < hi) { 
    var rest = range(lo + 1, hi); 
    return push(lo, rest); 
  }
  return nil();
}

function foldl(f, acc, xs){ 
  if null(xs){
    return acc;
  } else {
    var h      = head(xs);
    var accNew = f(acc, h); 
    return foldl(f, accNew, tail(xs));
  }
}

function minIndex(a){
   
   requires(length(a) > 0);
   ensures(0 <= $result && $result < (length(a)))
   
   function step(min, i){
     if (a[i] < a[min]) { return i } else { return min }
   }
   
   return foldl(step, 0, range(0, length(a)));
 }


