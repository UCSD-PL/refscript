
/*@ array_filter :: forall T. (a: #iArray[T], (x:T) => boolean) => {#iArray[T] | true} */
function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ is_num :: (x:number + undefined) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (#iArray[number]) => {#iArray[number] | true} */ 
function foo(arr:any, f:any) {
  return array_filter(arr, is_num);
}