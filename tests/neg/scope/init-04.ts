
/*@ foo :: (cnd: boolean) => { number | v > 0 } */
export function foo(cnd: boolean): number {
    /*@ local r :: number */
    let r = 0;
    if (cnd) {
        r = 1;
    }
    else {
        if (r > 5) {
            r = 10;
        }
        else {
            r = -2;
        }
    }
    return r;
}
