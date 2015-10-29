/*@ qualif UBound(v:number, x:a) : v < (len x) */

/*@ predicate within(v,a) = (0 <= v && v < (len a)) */

function forloop<A>(lo: number, hi: number, body: (x: number, y:A) => A, accum: A): A {
	if (lo < hi) {
		let newAcc = body(lo, accum);
		return forloop(lo + 1, hi, body, newAcc);
	}
	return accum;
}

/*@ minIndex :: ({a: IArray<number> | 0 < (len a)}) => {v:number | within(v,a) } */ 
function minIndex(a:number[]):number{

  /*@ readonly aa :: # */
  let aa = a; 

  // XXX : MAKE SURE THE NESTED FUNCTIONS arguments BINDS, INDEED SHADOW THE
  //       ENCLOSING FUNCTIONS ... 
	
	function step(i: number, min: number) {
  if ( aa[i]
  < 
  aa[min]) { 
			return i;
		} 
		return min; 
	}

	return forloop(0, aa.length, step, 0);

}
