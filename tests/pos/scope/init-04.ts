
/*@ foo :: (cnd: boolean) => { number | v > 0 } */
export function foo(cnd: boolean): number {
    /*@ local r :: number + undefined */
    let r;
    if (cnd) {
        r = 1;
    }
    else {
        if (r > 5) {
            r = -10;
        }
        r = 1;
    }
    return r;
}
