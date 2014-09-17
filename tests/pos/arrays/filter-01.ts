
/*@ is_num :: (x:number + undefined) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (#iArray[number]) => {#iArray[number] | true} */ 
function foo(arr:any, f:any) {
    return arr.filter(is_num);
}