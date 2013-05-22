

function minIndex(a){
   requires(length(a) > 0);
   ensures(0 <= $result && $result < length(a));
   var min = 0;
   var i   = 0;
   while(i < length(a)){
     invariant(0 <= min);
     invariant(min < length(a));
     invariant(0 <= i);
     if (a[i] < a[min]){ 
       min = i; 
     }
     i = i + 1;
   }
   return min;
}

/*@ type Rng A = {v:int | 0 <= v < length A}

/*@ loop :: ( a:{v:array int | 0 < (length v)}, min:Rng a) => Rng a  */
function loop(a, min, i){
 
  var minNext = min;
  if (i < length(a)) {
    if (a[i] < a[min]) { minNext = i; } 
    return loop(a, minNext, i+1)
  }
  
  return minNext;
}

/*@ minIndex :: (array int) => int */
function minIndex(a){
  requires(length(a) > 0);
  ensures(0 <= $result && $result < (length(a)))
  return loop(a, 0, 0);
}


function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

function minIndex(a){
  requires(length(a) > 0);
  ensures(0 <= $result && $result < (length(a)))
  function step(i, min){
    if (a[i] < a[min]) { return i } else { return min }
  }
  return forloop(0, length(a), step, 0);
}
////////////////////////////////////////////////////////////////
// EVERY number in output between lo ... hi

/*@ range :: (lo:int, hi:int) => list {v:int | lo <= v < hi}*/
function range(lo, hi) {
  if (lo < hi) { 
    var rest = range(lo + 1, hi); 
    return push(lo, rest); 
  }
  return nil();
}

/*@ foldl :: forall P Q. ((Q, P) -> Q), Q, list P) => Q */
function foldl(f, acc, xs){ 
  if nil(xs){
    return acc;
  } else {
    var h      = head(xs);
    var accNew = f(acc, h); 
    return foldl(f, accNew, tail(xs));
  }
}

/*@ minIndex :: (a:{v:array int | 0 < (length v)}) => v:int | 0 <= v && v < length(a)} */

 function minIndex(a){
  requires(length(a) > 0);
  ensures(0 <= $result && $result < length(a));
  
  function step(min, i){
    if (a[i] < a[min]) { return i } else { return min }
  }
  
  //P = {v:int | 0 <= v < len(a)} 
  //Q = {v:int | 0 <= v < len(a)} 
  var indices = range(0, length(a));
  return foldl(step, 0, indices);
}




