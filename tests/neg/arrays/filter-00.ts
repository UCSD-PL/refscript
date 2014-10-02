
/*@ array_filter :: forall T. (a: #Array[#Immutable, T], (x:T) => boolean) => {#Array[#Immutable, T] | true} */
function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ is_num :: (x:number) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (#Array[#Immutable, number + undefined]) => {#Array[#Immutable, number] | true} */ 
function foo(arr:any, f:any) {
  return array_filter(arr, is_num);
}
