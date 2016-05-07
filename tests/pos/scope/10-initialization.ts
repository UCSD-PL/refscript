
/*@ foo :: (cnd: boolean) => { number | v > 0 } */
export function foo(cnd: boolean): number {
    /*@ r :: number */
    let r = (cnd) ? 1 : 2;
    return r;
}
