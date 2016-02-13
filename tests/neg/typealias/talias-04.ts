/*@ predicate gt x y   = x >= y                    */
/*@ type Nat          = {number | gt(v, 0) }      */
/*@ type IArrayN<T,n> = {v:IArray<T> | len v = n} */

/*@ ga :: IArrayN<Nat, 3> */
let ga = [0,10,20,30];
