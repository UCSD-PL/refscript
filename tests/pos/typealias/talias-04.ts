/*@ predicate gt x y   = x >= y                    */
/*@ type nat          = {number | gt(v, 0) }      */
/*@ type Nat          = {number | gt(v, 0) }      */
/*@ type IArrayN<T,n> = {v:IArray<T> | len v = n} */
/*@ type FourNats     = IArrayN<Nat,4> */

/*@ a1 :: IArray<nat>    */
let a1 = [0, 10, 20, 30];

/*@ a2 :: FourNats       */
let a2 = [0, 10, 20, 30];

/*@ a3 :: IArrayN<Nat,4> */
let a3 = [0, 10, 20, 30];
