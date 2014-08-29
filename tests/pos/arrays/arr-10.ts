/*@ toNumber :: (x: string) => { number | 0 <= v }*/
function toNumber(x : string) : number {
	var n = Number(x);
	if (n >= 0) {
		return n;
	}
	else {
		return 0;
	}
}
