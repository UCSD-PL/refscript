/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ loop :: (list [number], number, number) => number */ 
function loop(b:number[], min:number, i:number):number{
	if (i < mylength(b)) {
		var min_:number = min;
		assert(i < mylength(b));
		if (nth(b, i) < nth(b, min)) { 
			min_ = i; 
		} 
		return loop(b, min_, i + 1)
	}
	return min;
}

/*@ minIndex :: ({a:list [number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a))} */ 
function minIndex(a){
	var r :number= loop(a, 0, 0);
	return r;
}


