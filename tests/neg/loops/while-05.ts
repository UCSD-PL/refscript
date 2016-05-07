/*@ foo :: (n: number) => { boolean | 0 < 1} */
function foo(n: number) {
    let b;
    let i;
    let j = 0;
    while (j < n) {
        b = i < 10;
        i = j;
        j++;
    }
    return b;
}
