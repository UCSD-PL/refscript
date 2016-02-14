
function idp<A>(x: A): A { return x; }

let f : (x: number) => number = idp;

assert(f(5) === 5) ;
