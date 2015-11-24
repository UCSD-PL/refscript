
/*@ is_num :: (x:number + undefined) => {boolean | 0 < 1} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number>) => { IArray<number> | 0 < 1 } */ 
function foo(arr:any) {
    return arr.filter(is_num);
}
