/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ forloop :: forall A. (number, number, (number, A) => A, A) => A */
function forloop(lo:number, hi:number, body:(number,any)=>any, accum:any):any{
	if (lo < hi) {
		var newAcc :any= body(lo, accum);
		return forloop(lo + 1, hi, body, newAcc);
	}
	return accum;
}

/*@ minIndex :: ({a: #Array[#Immutable, number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a))} */ 
function minIndex(a:number[]):number{
	
	/*@ step :: (number, number) => number */
	function step(i:number, min:number):number{
		if (a[i] < a[min]) { 
			return i;
		} 
		return min; 
	};
	return forloop(0, a.length, step, 0);
}
