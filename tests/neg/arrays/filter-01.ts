
/*@ is_num :: (x:number) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number + undefined>) => {IArray<number> | true} */ 
function foo(arr:any) {
    return arr.filter(is_num);
}
