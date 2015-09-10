/*@ foo :: forall T . (a: IArray<T>, 
                     indexes: IArray<{v: number | (0 <= v && v < (len a)) }>) 
                => { IArray<T> | (len v) = (len indexes) } */
declare function foo<T>(array: T[], indexes:number[]) : T[];

assert(foo([1,2,3],[2,1,0]).length === 3);
