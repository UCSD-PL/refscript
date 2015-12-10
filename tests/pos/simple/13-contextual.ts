/*@ qualif Lt3(v: int) : (v < 3) */
/*@ qualif Lt3(v: int) : (0 <= v) */
/*@ qualif Lt3(x: a, v: int) : v = len x */


/*@ foo :: <T>(a: IArray<T>, i: IArray<idx<a>>) => { IArray<T> | len v = len i } */
declare function foo_13<T>(a: IArray<T>, i: IArray<number>): IArray<T>;

let a_123 = [1, 2, 3];
/* local a_012 :: IArray<{number | 0 <= v && v < 3 }> */
let a_012 = [0, 1, 2];

assert(a_012.length === 3);

let f_13 = foo_13(a_123, a_012);

assert(f_13.length === 3);
