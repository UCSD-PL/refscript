
/*@ qualif PLusOne(v:number, w: number): v = w + 1   */

function inc(n: number) { return n + 1; }

var /*@ readonly */ x = { f: inc };

/*@ foo :: () => { number | v = 6 } */
function foo() :number {
  return (x.f)(5);
}
