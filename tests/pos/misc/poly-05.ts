
function idp<A>(x: A): A { return x; }

var f : (x: string) => string = idp;

assert(f("a") === "a");

