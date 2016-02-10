
/*@ listsum :: <M extends ReadOnly> (List<M,number>) => number */
function listsum<M extends ReadOnly>(xs:List<M,number>):number {
	let t = tail(xs);
	return listsum(t);
}

