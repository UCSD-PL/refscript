//adapted from navier-stokes


class Foo<M extends ReadOnly> {

    /*@ (Immutable) a: { number | v = 5 } */
    a = 5;

    constructor() { }

    /*@ bar({number | v = this.a}) : {number | v = 0} */
    bar(x) {
        return this.a - x;
    }
}

let z = new Foo();
z.bar(5);
