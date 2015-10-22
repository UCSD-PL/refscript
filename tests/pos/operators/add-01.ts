/*@ foo :: () => number */
export function foo():number
{
	let x:number = 0;
	let y:number = x + 1;
	assert(0 < y);
	return y
}
