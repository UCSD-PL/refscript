/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ range :: (number, number) => list [number] + null */
function range(lo, hi) {
  if (lo < hi) { 
    var rest = range(lo + 1, hi); 
    return cons(lo, rest); 
  }
  return nil();
}

/*@ foldl :: forall A B. ((A, B) => A, A, list[B] + null) => A */
function foldl(f, acc, xs){ 
  if (empty(xs)) {
    return acc;
  } else {
    var acc_ = f(acc, head(xs)); 
    return foldl(f, acc_, tail(xs));
  }
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
   
   return foldl(step, 0, range(0, length(a)));
}


