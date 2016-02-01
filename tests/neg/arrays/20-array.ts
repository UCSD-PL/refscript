
/*@ qualif Length(v: a): len v = 3 */
/*@ qualif Length(v: a): len v = 8 */

let a1: IArray<number> = [1,2,3];
let a2: IArray<number> = [4,5,6];
let a3: IArray<number> = a1.concat(a2);
assert(a3.length === 8);
