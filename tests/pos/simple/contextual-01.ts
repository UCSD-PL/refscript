
/*@ qualif ArrLen(v: a) : (len v) = 1 */

/*@ foo :: forall T . (a: #Array[#Immutable, T], 
                     indexes: #Array[#Immutable, {v: number | (0 <= v && v < (len a)) }]) 
                => { #Array[#Immutable, T] | (len v) = (len indexes) } */
declare function foo<T>(array: T[], indexes:number[]) : T[];



assert(foo([1,2,3],[2,1,0]).length === 3);
