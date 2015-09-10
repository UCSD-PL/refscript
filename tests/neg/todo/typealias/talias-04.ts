/*@ predicate gt x y   = x >= y                    */
/*@ alias Nat          = {number | gt(v, 0) }      */
/*@ alias IArrayN<T,n> = {v:IArray<T> | len v = n} */

/*@ ga :: IArrayN<Nat, 3> */
var ga = [0,10,20,30];
