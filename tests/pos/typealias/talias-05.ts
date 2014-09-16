/*@ predicate gt x y   = x >= y                       */
/*@ alias Nat          = {number | 0 <= v}            */
/*@ alias iArray[T]    = #Array[#Immutable, T]        */
/*@ alias mArray[T]    = #Array[#Mutable, T]          */
/*@ alias iArrayN[T,n] = {v:#iArray[T] | len v = n}   */

/*@ alias pair[T]      = {v: #iArray[T] | len(v) = 2} */

// Should not need this qualifier, it is in the output type!
/* qualif Pair(v:a): len(v) = 2 */

/*@ foo :: (number, number) => #iArray[#pair[number]] */
function foo(x:number, y:number) {
    var z = [x, y];
    return [z];
}