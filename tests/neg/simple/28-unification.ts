//adapted from transducers

class Foo<M extends ReadOnly, T> {
    constructor() { }
}

/*@ reduce ::    (Foo<Immutable, boolean>, string) => { void | 0 < 1 } */
/*@ reduce :: <T>(Foo<Immutable, T>,       number) => { void | 0 < 1 } */
export function reduce(xf, coll) {
    if (typeof coll === "number") {
        /*@ xxf :: Foo<Immutable, boolean> */
        let xxf = xf;
    }
}
