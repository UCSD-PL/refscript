
/*@ foo :: (cnd: boolean) => { number | v > 0 } */
export function foo(cnd: boolean): number {
    /*@ r :: number */
    let r;
    if (cnd) {
        r = 1;
    }
    else {
        r = 2;
    }
    return r;
}
