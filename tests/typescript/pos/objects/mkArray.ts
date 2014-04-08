
/*@ mkArray :: (number) => [{number | 0 <= v}] */
function mkArray(n: number): number[] {
	var i = 0;
	var a = [];
	while (i < n){
		a.push(i);
	}
	return a;
}
