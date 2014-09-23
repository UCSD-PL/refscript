/*@ predicate gt x y   = x >= y                    */
/*@ alias nat          = {number | gt(v, 0) }      */
/*@ alias Nat          = {number | gt(v, 0) }      */
/*@ alias IArray<T>    = Array<Immutable, T>       */
/*@ alias IArrayN<T,n> = {v:IArray<T> | len v = n} */


/*@ a1 :: IArray<nat>    */
var a1 = [0, 10, 20, 30];


/*@ a2 :: IArrayN<Nat,4> */
var a2 = [0, 10, 20, 30];

