/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ predicate within(v,a) = (0 <= v && v < (len a)) */

function forloop<A>(lo: number, hi: number, body: (x: number, y:A) => A, accum: A): A {
	if (lo < hi) {
		var newAcc = body(lo, accum);
		return forloop(lo + 1, hi, body, newAcc);
	}
	return accum;
}

/*@ minIndex :: ({a: #Array[#Immutable, number] | 0 < (len a)}) => {v:number | within(v,a) } */ 
function minIndex(a:number[]):number{
	
  /* step :: (i: { number | within(v,a) }, min: { number | within(v,a) }) 
           => { number | within(v,a) } 
   */
	function step(i: number, min: number) {
		if (a[i] < a[min]) { 
			return i;
		} 
		return min; 
	};
	return forloop(0, a.length, step, 0);
}
