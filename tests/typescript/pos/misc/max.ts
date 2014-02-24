/*@ max :: (number, number) => number */ 
function max(x:number, y:number):number{ 
	var r :number= 0;
	if (x > y) {
		r = x;
	} else {
		r = y;
	}
	assert(r >= x);
	assert(r >= y);
	return r;
}
