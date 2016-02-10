/*@ sum :: <M extends ReadOnly> (LList<M,{v:number| 0 <= v}>) => number */
function sum<M extends ReadOnly>(xs : List<M,number>) : number{
	if (empty(xs)) {
		return 0;
	}
	let h : number   = head(xs);
	let t : List<M,number> = tail(xs);
	let z : number   = sum(xs);

	return 10; // h + sum(t);
}
