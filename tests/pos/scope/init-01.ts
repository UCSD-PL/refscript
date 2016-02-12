
/*@ foo :: (cnd: boolean) => { number | v > 0 } */
export function foo(cnd: boolean): number {
    let r: number = (cnd) ? 1 : 2;
    return r;
}
