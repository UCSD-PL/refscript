/*@ map :: <A,B,M extends ReadOnly> ((A) => B, LList<M,A>) => LList<M,B> */
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

/*@ listsum :: <A,B,M extends ReadOnly> (LList<M,number>) => number */
function listsum(xs){
  if (empty(xs)) {
    return 0;
  }
  let h = head(xs);
  let t = tail(xs);
  return h + listsum(t);
}

/*@ main :: (number) => {v:number | v >= 0} */
function main(n){
  let as = cons(n, cons(n+1, cons(n+2, nil())));
  let bs = map(abs, as);
  let r  = listsum(bs);
  assert(r >= 0);
  return listsum(bs);
}
