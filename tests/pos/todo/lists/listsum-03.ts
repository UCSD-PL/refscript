
/*@ listsum :: <M extends ReadOnly> (LList<M,number>) => number */
function listsum(xs:number[]):number {
	let t :List<number> = tail(xs);
	return listsum(t);
}

