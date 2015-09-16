
/*@ foo :: <T>(a: IArray<T>, indices: IArray<idx<a>>) => { IArray<T> | len v = len indices } */
declare function foo<T>(array: IArray<T>, indices: IArray<number>): IArray<T>;

assert(foo([1, 2, 3], [2, 1, 0]).length === 3);
