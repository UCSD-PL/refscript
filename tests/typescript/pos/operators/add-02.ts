/*@ foo :: (number) => number */
function foo(x:number):number{
	var y :number= 0;
	if (0 < x) {
		y = x + y;
		assert (0 <= y);
	}
	return y;
}
