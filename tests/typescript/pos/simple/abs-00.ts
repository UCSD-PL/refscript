/*@ ab :: (number) => {res: number | res >= 10} */

function ab(x:number):number{
	var r = x;
	if (x > 0) {
		r = x;
	} else {
		r = (0 - x);
	}
	return r;
}
