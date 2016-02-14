
/*@ qualif Bot(v: number): v = 5 */
/*@ qualif Bot(v: number): v = 6 */

function idp<A>(x: A): A { return x; }

let f = <(x: number) => number> idp;

assert(f(5) === 6);
