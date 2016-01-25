/*@ qualif Length(v: a): len v = 3 */

/*@ a :: IArray<number> */
let a = [1,2,3];

/*@ b :: { IArray<number> | len v = 4 } */
let b = [1,2,3,4];

assert(a.length + b.length === 7);
