
/*@ qualif Ineq3(v : number ): (5 <= v) */

/*@ foo :: (x: (Mutable) { a: number }) => number */
export function foo(x) : number {
	for (let i = 0; i < 5; i ++) {
		x.a = i;
	}
	return x.a;
}
