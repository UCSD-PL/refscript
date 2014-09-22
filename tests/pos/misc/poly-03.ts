
/* qualif Cmp(v:number): v = 5 */

// FIXME: why is this not working without the refinement?

/*@ idp :: forall A . (x: A) => { A | v = x }  */
function idp<A>(x: A): A { return x; }

var f = <(x: number) => number> idp;

assert(f(5) === 5);

