
/*@ array_filter :: forall T. (IArray<T>, (x:T) => boolean) => {IArray<T> | true} */
function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ is_num :: (x:number) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number + undefined>) => {IArray<number> | true} */ 
function foo(arr:any, f:any) {
  return array_filter(arr, is_num);
}
