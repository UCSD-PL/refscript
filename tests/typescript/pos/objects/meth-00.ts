
/*@ mkArray :: (number) => [{number | 0 <= v}] */
function mkArray(n:number):number[]{
	var i:number = 0;
	var a :number[] = [];
	while (i < n){
		a.push(i);
	}
	return a;
}
