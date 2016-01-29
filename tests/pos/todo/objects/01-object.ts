
/*@ qualif PLusOne(v: int, w: int): v = w + 1   */
/*@ qualif Six(v: int): v = 6   */

function inc1(n: number) { return n + 1; }
function inc2(n: number) { return n + 2; }

/*@ xx :: { @Final f: (n: number) => number } */
let xx = { f: inc1 };

function foo(): number {
    return xx.f(5);
}

assert(foo() === 6);
