
//adapted from navier-stokes

class Foo<M extends ReadOnly> {
    /*@ (Immutable) a: number */
    a;

    /*@ new (x: number): { v: Foo<M> | this.a = x } */
    constructor(x) {
        this.a = x;
    }

    /*@ bar(x: {number | v = this.a}): {number | v = 0} */
    bar(x) {
        return this.a - x;
    }
}

let z = new Foo(5);
z.bar(5);
