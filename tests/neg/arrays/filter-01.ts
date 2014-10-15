
/*@ is_num :: (x:number) => {boolean | true} */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (#Array[#Immutable,number + undefined]) => {#Array[#Immutable,number] | true} */ 
function foo(arr:any, f:any) {
    return arr.filter(is_num);
}
