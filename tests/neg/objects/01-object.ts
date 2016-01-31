
/*@ qualif PLusOne(v:int, w: int): v = w + 1   */
/*@ qualif Six(v:int): v = 6   */

function inc1(n: number) { return n + 1; }
function inc2(n: number) { return n + 2; }

/*@ x :: { @Final f: (n: number) => number } */
let x = { f: inc2 };

function foo() :number {
  return x.f(5);
}

assert(foo() === 6);
