
/*@ foo :: (cnd: boolean) => number + undefined */
export function foo(cnd: boolean): number {
    /*@ r :: number */
    let r;
    if (cnd) {
        r = 1;
    }
    return r;
}
