/*@ map :: forall A B. ((A) => B, #List[A]?) => #List[B]? */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(head(xs)), map(f, tail(xs)));
}

/*@ abs :: (number) => number */
function abs(x){
  if (x <= 0){
    return (-x);
  }
  return x;
}

/*@ listsum :: (#List[number]?) => number */
function listsum(xs){
  if (empty(xs)) {
    return 0;
  }
  var h = head(xs);
  var t = tail(xs);
  return h + listsum(t);
}

/*@ main :: (number) => {v:number | v >= 0} */
function main(n){
  var as = cons(n, cons(n+1, cons(n+2, nil())));
  var bs = map(abs, as);
  var r  = listsum(bs);
  assert(r >= 0);
  return listsum(bs);
}
