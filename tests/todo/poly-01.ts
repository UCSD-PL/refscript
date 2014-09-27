/*@ alias iArray[T] = #Array[#Immutable,T] */

/*@ array_filter :: forall T. (a: #iArray[T], (x:T) => boolean) => {#iArray[T] | true} */
function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ array_map :: forall T U. (a: #iArray[T], (x:T) => U) => {#iArray[U] | true} */
function array_map(a, f) {
    return array_map(a, f);
}

/*@ foo :: forall A. (#iArray[A], (x:A) => string) => {#iArray[string] | true} */ 
function foo<A>(arr:A[], f:(x:A) => string) : string[] {
    var arr1 = array_map(arr, f);
    return arr1;
} 