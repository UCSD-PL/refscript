/*@ map :: forall A B. ((A) => B, list [A]) => list [B] */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(head(xs)), map(f, tail(xs)));

  // var x0  = head(xs);
  // var xs_ = tail(xs);
  // var y   = f(x0);
  // var ys_ = map(f, xs_);
  // return cons(y, ys_);

}

/*@ abs :: (number) => number */
function abs(x){
  if (x <= 0){
    return (x);
  }
  return x;
}

/*@ listsum :: (list [number]) => number */
function listsum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  return h + listsum(t);
}

/*@ main :: ({n: number | true}) => {v:number | v >= 0} */
function main(n){
  var as = cons(n, cons(n+1, cons(n+2, nil())));
  var bs = map(abs, as);
  var r  = listsum(bs);
  assert(r >= 0);
  return listsum(bs);
}
