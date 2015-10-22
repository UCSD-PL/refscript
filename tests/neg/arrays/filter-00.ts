
/*@ array_filter :: forall T. (IArray<T>, (x:T) => boolean) => {IArray<T> | 0 < 1} */
function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ is_num :: (x:number) => {boolean | 0 < 1} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number + undefined>) => {IArray<number> | 0 < 1} */ 
function foo(arr:any, f:any) {
  return array_filter(arr, is_num);
}
