
function idp<A>(x: A): A { return x; }

/*@ f :: (number) => number */
var f = <(number) => number> idp;

assert(f(6) === 5) ;

