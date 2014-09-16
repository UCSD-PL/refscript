/*@ predicate gt x y = x >= y                */
/*@ alias Nat        = {number | 0 <= v}     */
/*@ alias iArray[T]  = #Array[#Immutable, T] */

/*@ ga :: #iArray[#Nat] */
var ga = [0,0,0,0];
