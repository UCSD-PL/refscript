
/*@ foo :: <T>(a: IArray<T>, is: IArray<idx<a>>) => { IArray<T> | len v = len is } */
declare function foo<T>(array: T[], is:number[]) : T[];

assert(foo([1,2,3],[2,1,0]).length === 2);
