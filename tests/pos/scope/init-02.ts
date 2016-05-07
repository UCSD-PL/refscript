
/*@ foo :: (cnd: boolean) => posint */
export function foo(cnd: boolean): number {
    /*@ local r :: number + undefined */
    let r;
    if (cnd) {
        r = 1;
    }
    else {
        r = 2;
    }
    return r;
}
