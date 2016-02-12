/*@ map :: <A,B,M extends ReadOnly> ((A) => B, LList<M,A>) => LList<M,B> */
function map(f, xs){
	if (empty(xs)) {
		return nil();
	}
	// return cons(f(head(xs)), map(f, tail(xs)));
	
	let x0  = head(xs);
	let xs_ = tail(xs);
	let y   = f(x0);
	let ys_ = map(f, xs_);
	return cons(y, ys_);

}

/*@ abs :: (number) => {v:number | 0 <= v} */
function abs(x){
  if (x <= 0){
    return (0 - x);
  }
  return x;
}

/*@ main :: <M extends ReadOnly> (LList<M,number>) => LList<M,{v:number | 0 <= v}> */
function main(xs){
  let bs = map(abs, xs);
  return bs;
}
