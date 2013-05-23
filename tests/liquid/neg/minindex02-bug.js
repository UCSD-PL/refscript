/*@ qualif UBound(v:int, x:a) : v < (len x) */

/*@ range :: (int, int) => list [int] */
function range(lo, hi) {
  if (lo <= hi) { 
    var rest = range(lo + 1, hi); 
    return cons(lo, rest); 
  }
  return nil();
}

/*@ foldl :: forall A B. ((A, B) => A, A, list[B]) => A */
function foldl(f, acc, xs){ 
  if (empty(xs)) {
    return acc;
  } else {
    var acc_ = f(acc, head(xs)); 
    return foldl(f, acc_, tail(xs));
  }
}

/*@ minIndex :: ({a:list [int] | 0 < (len a)}) => {v:int | (0 <= v && v < (len a))} */ 
function minIndex(a){
   
   /*@ step :: (int, int) => int */
   function step(i, min){
     if (nth(a, i) < nth(a, min)) { 
       return i;
     } 
     return min; 
   };
   
   return foldl(step, 0, range(0, length(a)));
 }


