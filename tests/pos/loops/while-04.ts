/*@ qualif Ineq1(v : int ): (v <= 5) */
/*@ qualif Eq1(v: int, i: int): v = i - 1 */

/*@ foo :: () => { number | v = 4 } */
function foo() : number{
	let x = 0;
	let i = 1;
	assert(x === i - 1);
	while (i < 5) {
      x = i;
		i = i + 1;
	}
	assert(i === 5);
	assert(x < i);
	assert(x === i - 1);
	return x;
}
