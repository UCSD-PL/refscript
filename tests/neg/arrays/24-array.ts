/*@ qualif Eq5(v: int): v > 0 */

// /  mkArray :: (n: number) => MArray<posint> */
// export function mkArray(n: number): MArray<number> {
// 	let i = 1;
//
// 	/*  a :: MArray<posint> */
// 	let a: MArray<number> = [];
//
// 	while (i > (-10)) {
// 		a.push(i);
// 		i = i - 1;
// 	}
// 	return a;
// }



/*@ foo :: (n: { number | v > 5}) => void */
declare function foo(n:number): void;


	var i = 10;
	while (i >  0) {
		foo(i);
		i = i - 1;
	}
