
/*@ is_num :: (x:number + undefined) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (#Array[#Immutable,number]) => {#Array[#Immutable,number] | true} */ 
function foo(arr:any, f:any) {
    return arr.filter(is_num);
}
