
/*@ map :: forall A B. ((A) => B, list [A]) => [B] */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}

/*@ append :: forall A. (list [A], list [A]) => list [A] */
function append(xs, ys){
  if (empty(xs)) {
    return ys;
  } else {
    var x   = safehead(xs);
    var xs_ = safetail(xs);
    return cons(x, append(xs_, ys));
  }
}

/*@ reverse :: forall A. (list [A]) => list [A] */
function reverse(xs){
  function go(acc, ys){
    if (empty(ys)){
      return acc;
    }
    var y    = safehead(ys);
    var ys_  = safetail(ys);
    var acc_ = cons(y, acc);
    return go(acc_, ys_);
  }
  return go(nil(), xs);
}

function makeList(n){
  if (n <= 0){
    return nil();
  }
  return cons(random(), makeList(n-1));
}

/*@ abs :: (int) => int */
function abs(x){
  var r = x;
  if (x <= 0) {
    r = -x;
  }
  return r;
}

/*@ main :: {n:int | n > 0} => void */
function main(n){
  // Generate: Two random lists
  var xs = makeList(n);
  var ys = makeList(n);
  // Property: map preserves lengths
  var as = map(abs, xs);
  assert(length(as) == length(xs));
  // Property: reverse preserves lengths
  var bs = reverse(ys);
  assert(length(bs) == length(ys));
  // Property: append adds lengths
  var cs = append(xs, ys);
  assert(length(cs) == (length(xs) + length(bs)));
}
  

