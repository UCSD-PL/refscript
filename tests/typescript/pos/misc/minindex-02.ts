/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ range :: (number, number) => #Array[#Immutable, number] */
function range(lo:number, hi:number) :number[]{
	if (lo < hi) { 
		var rest :number[]= range(lo + 1, hi); 
		return [lo].concat(rest); 
	}
	return [];
}

/*@ foldl :: forall A B. ((A, B) => A, A, #Array[#Immutable,B]) => A */
function foldl(f, acc, xs){ 
	if (xs.length === 0) {
		return acc;
	} else {
		var acc_ = f(acc, xs[0]); 
		return foldl(f, acc_, xs.slice(1,xs.length));
	}
}

/*@ minIndex :: ({a:#Array[#Immutable, number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a)  )} */ 
function minIndex(a){
	
	/*@ step :: (number, number) => number */
	function step(i, min){
		if (a[i] < a[min]) { 
			return i;
		} 
		return min; 
	};
	
	return foldl(step, 0, range(0, a.length));
}


