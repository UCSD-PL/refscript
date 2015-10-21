
/*@ foo :: (n: number) => { number | true } + undefined */
export function foo(n: number): number {
    /*@ r :: number */
    let r;
    while (n < 10) {
        r = 1;
        n++;
    }
    return r;
}
