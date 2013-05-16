function min_index(a){
  function loop(min, i){
    var min_next = min;
    if (i < length(a)) {
      if (a[i] < a[min]) { min_next = i; } 
      return loop(min_new, i+1)
    }
    return min;
  }
  return loop(0, 0);
}

/*@ min_index :: (array int) => int */
/*@ min_index :: (a:{v:array int | 0 < (len v)}) => {v:int | 0 <= v < (len a)} */

/*@ loop :: (int, int) => int */
/*@ loop :: (min:{v:int|0 <= v < (len a)}, i:{v:int|0 <= i}) => {v:int | 0 <= v < (len a)} */

/*@ abs :: (int) => Int */
/*@ abs :: (x:int) => {v:Int | v > 0} */

function abs(x){
  var res = 0;
  if (x > 0) {
    res = x;
  } else {
    res = 1-x;
  };
  return res;
}


// First order EASY

// 1. Show type checking
// *. Formalize: functions/calls/assign/if-then-else/primop
//
// 2. Show ref-type checking
// 3. Motivate need for SINGLETON [o.w. can't prove type for abs]
// 4. Motivate need for SSA by res = x ~~~~> {z = res; res = x;}
// *  Formalize RefTyping : functions/calls/assign/if-then-else/primop


// First order INVOLVED
// 5. Min-Index
// 6. Type-Checking
// 7. Ref-Type-Checking

// Higher-Order MONO [FUNCTION SUBTYPING]

/*@ forloop :: (int, int, (int, int) => int, int) => int */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}

/*@ define Rng A = {v:int | 0 <= v (len A)} */
/*@ min_index :: (a:{v:array int | 0 < (len v)}) => {v:int | 0 <= v < (len a)} */
/*@ min_index :: (a:{v:array int | 0 < (len v)}) => (Rng a) */
/*@ step :: (i:{v:int | 0 <= v < (len a)}, min:{v:int | 0 <= v < (len a)}) => {v:int | 0 <= v < (len a)} */
/*@ step :: (i:(Rng a), min:(Rng a)) => (Rng a) */

function min_index(a){
  function step(i, min){
    var min_next = min;
    if (a[i] < a[min]){ 
      min_next = i; 
    }
    return min_next;
  }
  return forloop(0, length(a), step, 0);
}

// Higher-Order POLY [Instantiation]

/*@ forloop :: forall A. (int, int, (int, A) => A, A) => A */
function forloop(lo, hi, body, acc){
  if (lo < hi) {
    var newAcc = body(lo, acc);
    return forloop(lo + 1, hi, body, newAcc);
  }
  return acc;
}


function min_index(a){
  function step(i, min){
    var min_next = min;
    if (a[i] < a[min]){ 
      min_next = i; 
    }
    return min_next;
  }
  return forloop/*@[(Rng a)]*/(0, length(a), step, 0);
}

// --------------------------------------------------------------------------
// Next: Collections, Poly, Fold ...

