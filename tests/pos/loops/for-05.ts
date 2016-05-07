
/*@ qualif Poo(v: int, i: int): v = i - 1 */
/*@ qualif Poo(v: int): v < 5 */

/*@ bar :: () => { number | 4 = v } */
function bar(): number {
	let z = -1;
	for (let i = 0; i < 5; i++) {
		z = i;
	}
	return z;
}
