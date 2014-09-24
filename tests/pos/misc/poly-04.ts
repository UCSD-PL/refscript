
function idp<A>(x: A): A { return x; }

/*@ f :: (number) => number */
var f : (x: number) => number = idp;

assert(f(5) === 5) ;

