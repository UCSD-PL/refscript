/*@ qualif Eq5(v: int): v > 0 */

/*@ mkArray :: (n: number) => MArray<posint> */
export function mkArray(n: number): MArray<number> {
	let i = 1;
	/*@ a :: MArray<posint> */
	let a: MArray<number> = [];
	while (i < n) {
		a.push(i--);
		// assert(false);
	}
	return a;
}
