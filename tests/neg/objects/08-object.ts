/*@ qualif PLusOne(v: int, w: int): v = w + 1 */
/*@ qualif Eq5(v: int): v = 5 */
/*@ qualif Eq6(v: int): v = 6 */

function inc(n: number): number { return n + 1; }

/*@ readonly g09 :: {
    @Final a: number;
    @Final f: (n: number) => number;
} */
let g09 = {
    a: 5,
    f: inc
};

/*@ foo :: () => { number | v = 7 } */
function foo() {
    let gf = g09.f;
    return gf(g09.a);
}
