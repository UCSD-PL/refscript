
/*@ listsum :: (#List [number]) => number */
function listsum(xs:number[]):number {
	var t :number[]= tail(xs);
	return listsum(t);
}

