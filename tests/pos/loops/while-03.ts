/*@ qualif Ineq1(v : int ): (v <= 5) */

/*@ foo :: () => { number | v = 5 } */
function foo() : number {
	let x = 0;
	let i = 1;
	while (i < 5) {
		x = i;
		i = i + 1;
	}
	return i;
}
