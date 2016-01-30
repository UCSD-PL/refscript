
/*@ is_num :: (x: number + undefined) => boolean */
function is_num(x:any) {
    return !isNaN(x);
}

/*@ foo :: (IArray<number>) => IArray<number> */
export function foo(arr:any) {
    return arr.filter(is_num);
}
