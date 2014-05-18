/*@ toNumber :: (x: string) => { number | 0 <= v }*/
function toNumber(x : string) : number {
	var n : number= Number(x);
	if (n >= 0) {
		return n;
	}
	else {
		return 0;
	}
}


/*@ foo :: (#Array[#Immutable,string]) => #Array[#Immutable, { number | 0 <= v } ] */
function foo(arr : string[]) : number[] {
  return arr.map(toNumber);
}
