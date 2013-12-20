/*@ map :: forall A B. ((A) => B, list[A] + null) => list[B] + null */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  // return cons(f(head(xs)), map(f, tail(xs)));

  var x0  = head(xs);
  var xs_ = tail(xs);
  var y   = f(x0);
  var ys_ = map(f, xs_);
  return cons(y, ys_);

}

/*@ abs :: (number) => {v:number | 0 <= v} */
function abs(x){
  if (x <= 0){
    return (0 - x);
  }
  return x;
}

/*@ main :: (list[number] + null) => list[{v:number | 0 <= v}] + null */
function main(xs){
  var bs = map(abs, xs);
  return bs;
}
