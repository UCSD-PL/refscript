/*@ qualif Lt3(v: int) : (v < 3) */


/*@ foo :: <T>(a: IArray<T>, i: IArray<idx<a>>) => { IArray<T> | len v = len i } */
declare function foo_13<T>(a: IArray<T>, i: IArray<number>): IArray<T>;

/*@ readonly */ let a_123 = [1, 2, 3];

/*  readonly a_012 :: { IArray<idx<a_123>> | len v = 3 } */
let a_012 = [0, 1, 2];

assert(a_012.length === 3);

let f_13 = foo_13(a_123, a_012);

assert(f_13.length === 3);
