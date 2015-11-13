
/*@ foo :: () => {number | 0 < 1} */
function foo():number {
	var x = 0;
	var y = + x;
	return y
}
