
function idp<A>(x: A): A { return x; }

/*@ f :: (number) => number */
var f : (x: number) => number = idp;

assert(f(<any>"a") === <any>"a");

