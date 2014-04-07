/*@ listsum :: (#List [{v:number | 0 <= v}] + null) => {v:number | 0 <= v} */
function listsum(xs : number[]) : number{
	if (empty(xs)) {
		return 0;
	}
	var h :number   = head(xs);
	var t :number[] = tail(xs);
	return h + listsum(t);
}

