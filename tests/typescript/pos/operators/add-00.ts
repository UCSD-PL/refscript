
/*@ foo :: () => number */
function foo():number
{
	var x :number= 0;
	var y :number= x + 1;
	assert(0 < y);
	return y
}
