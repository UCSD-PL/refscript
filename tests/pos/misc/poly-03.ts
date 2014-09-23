
/*@ qualif Cmp(v:number): v = 5 */

function idp<A>(x: A): A { return x; }

var f = <(x: number) => number> idp;

assert(f(5) === 5);

