
/*@ qualif PLusOne(v:number, w: number): v = w + 1   */
/*@ qualif Six(v:number): v = 6   */

function inc1(n: number) { return n + 1; }
function inc2(n: number) { return n + 2; }

var x /*@ readonly */ = { f: inc1 };

function foo() :number {
  return (x.f)(5);
}

// x = { f: inc2 };

assert(foo() === 6);

