/*@ qualif PLusOne(v: int, w: int): v = w + 1 */
/*@ qualif Eq5(v: int): v = 5 */

function inc(n: number): number { return n + 1; }

/*@ readonly gg :: (Immutable) { a: number; f: (n: number) => number; } */
let gg = { a: 5, f: inc };


module A {

    /*@ foo :: () => { number | v = 6 } */
    function foo() {
        let gf = gg.f;
        return gf(gg.a);
    }

}
