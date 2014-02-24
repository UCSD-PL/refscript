/*@ map :: forall A B. ((A) => B, xs: { [A] | 0 <= (len v)}) => { [B] | (len v) = 0 } */
function map(f, xs) {
	if (empty(xs)) {
		return nil();
	}
	//return cons(f(head(xs)), map(f, tail(xs)));
	var x0  = head(xs);
	var xs_ = tail(xs);
	var y   = f(x0);
	var ys_ = map(f, xs_);
	return cons(y, ys_);

	//assert(false);
  
}


