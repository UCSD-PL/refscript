/*@ listsum :: <M extends ReadOnly> (LList<M,{ number | 0 <= v }>) => { number | 0 <= v } */
function listsum<M extends ReadOnly>(xs : List<M, number>) : number{
	if (empty(xs)) {
		return 0;
	}
	let h :number   = head(xs);
	let t :List<M, number> = tail(xs);
	return h + listsum(t);
}
