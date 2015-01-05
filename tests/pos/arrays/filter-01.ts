
/*@ is_num :: (x:number + undefined) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number>) => { IArray<number> | true } */ 
function foo(arr:any) {
    return arr.filter(is_num);
}
