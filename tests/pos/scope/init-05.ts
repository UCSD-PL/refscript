
/*@ foo :: (cnd: boolean) => number + undefined */
export function foo(cnd: boolean): number {
    /*@ local r :: number + undefined */
    let r;
    if (cnd) {
        r = 1;
    }
    return r;
}
