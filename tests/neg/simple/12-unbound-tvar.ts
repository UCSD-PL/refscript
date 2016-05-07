
/*@ foo :: (a: IArray<T>) =>  { v: number | v >= 0 && v = (len a) } */
export declare function foo<T>(a: IArray<T>): number;

let a = foo([1,2]);
