/*@ foo :: forall T . (a: IArray<T>, 
                     indexes: IArray<{v: number | (0 <= v && v < (len a)) }>) 
                => { IArray<T> | (len v) = (len indexes) } */
declare function foo<T>(array: T[], indexes:number[]) : T[];

/*@ as :: {IArray<number> | len v = 3} */
var as = [1,2,3];

/*@ is :: {IArray<{number | 0 <= v && v < 3}> | len v = 3} */
var is = [2,1,0];


assert(foo(as, is).length === 3);
