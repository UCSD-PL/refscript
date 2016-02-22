//adapted from transducers

class Bar<M extends ReadOnly> {
    constructor(x: string);
    /*@ new (x: posint): Bar<M> */
    constructor(x: number);
    constructor(x: any) {

    }
}

class Foo<M extends ReadOnly, T> extends Bar<M> {
    constructor() {
        super(1);
    }
}

/*@ reduce ::    (Foo<Immutable, boolean>, string) => void */
/*@ reduce :: <T>(Foo<Immutable, T>,       number) => void */
export function reduce(xf, coll) {
    if (typeof coll === "number") {
        /*@ xxf :: Foo<Immutable, boolean> */
        let xxf = xf;
    }
}
