
function idp<A>(x: A): A { return x; }

/*@ f :: (x: number) => number */
let f = <(x: number) => number> idp;

assert(f(6) === 5) ;
