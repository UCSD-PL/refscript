/*@ sum :: (#List [{v:number| 0 <= v}]) => number */
function sum(xs : number[]) : number{
	if (empty(xs)) {
		return 0;
	}
	var h : number   = head(xs);
	var t : number[] = tail(xs);
	var z : number   = sum(xs);

	return 10; // h + sum(t);
}

