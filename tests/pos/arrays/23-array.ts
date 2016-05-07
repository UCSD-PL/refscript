
/*@ qualif Length(v: a): len v = 6 */
/*@ qualif LtN(v: int): v < 7 */

let a: IArray<number> = [1,2,3,4,5,6]

assert(a[5] <= 10);
