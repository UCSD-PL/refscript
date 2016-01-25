
/*@ qualif Length(v: int): len v = 3 */
/*@ qualif Length(v: int): len v = 4 */

let a: IArray<number> = [1,2,3];

/*@ b :: { IArray<number> | len v = 4 } */
let b = [1,2,3,4];

assert(a.length + b.length === 8);
