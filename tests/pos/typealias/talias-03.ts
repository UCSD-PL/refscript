/*@ predicate gt x y   = x >= y                     */
/*@ type Nat          = {number | 0 <= v}          */
/*@ type iArrayN<T,n> = {v: IArray<T> | len v = n } */

/*@ ga :: IArray<Nat> */
let ga = [0,10,20,30];
