declare function NumberC (x : string) : number; 

/*@ toNumber :: (x: string) => { number | 0 <= v }*/
function toNumber(x : string) : number {
	var n : number= NumberC(x);
	if (n >= 0) {
		return n;
	}
	else {
		return 0;
	}
}


/*@ foo :: ([string]) => [ { number | 0 <= v } ] */
function foo(arr : string[]) : number[] {
  return arr.map(toNumber);
}
