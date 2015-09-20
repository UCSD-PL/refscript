//adapted from transducers
class Foo<T> { constructor(){} }
/*@ reduce :: /\    (Foo<Immutable, boolean>, string) => void
              /\ <T>(Foo<Immutable, T>,       number) => true */


function reduce(xf, coll) {
    if (typeof coll === "string") {
        /*@ xxf :: Foo<Immutable, boolean> */
        var xxf = xf;
    }
}
