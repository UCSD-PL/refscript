
/*@ foo :: <T>(T, {v:number | v > 0}) => T + undefined */
function foo<T>(x: T, n: number) {
    let i = n;

    /*@ a :: T + undefined */
    let a = undefined;

    while (0 < i) {
        if (i > 100) {
            a = x;
        } else {
            a = undefined;
        }
        i--;
    }

    return a;
}
