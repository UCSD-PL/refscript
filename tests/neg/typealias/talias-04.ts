/*@ predicate gt x y   = x >= y                     */
/*@ alias Nat          = {number | 0 <= v}          */
/*@ alias iArray[T]    = #Array[#Immutable, T]      */
/*@ alias mArray[T]    = #Array[#Mutable, T]        */
/*@ alias iArrayN[T,n] = {v:#iArray[T] | len v = n} */


/*@ ga :: #iArrayN[#Nat, 3] */
var ga = [0,10,20,30];
