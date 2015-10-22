
/*@ toNumber :: (x: string) => { number | 0 <= v } */
function toNumber(x) {
    let n = Number(x);
    if (n >= 0) {
        return n;
    }
    else {
        return 0;
    }
}


/*@ foo :: (IArray<string>) => IArray<{ number | 0 <= v }> */
export function foo(arr) {
    return arr.map(toNumber);
}
