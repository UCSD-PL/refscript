/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ loop :: (#Array[#Immutable,number], number, number) => number */ 
function loop(b:number[], min:number, i:number):number{
	if (i < b.length) {
		var min_:number = min;
		assert(i < b.length);
		if (b[i] < b[min]) { 
			min_ = i; 
		} 
		return loop(b, min_, i + 1)
	}
	return min;
}

/*@ minIndex :: ({a: #Array[#Immutable,number] | 0 < (len a)}) => {v:number | (0 <= v && v < (len a))} */ 
function minIndex(a){
	var r :number= loop(a, 0, 0);
	return r;
}


