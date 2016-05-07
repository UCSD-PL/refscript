//adapted from transducers

class Foo<M extends ReadOnly> {
    constructor() {}
}
class Super<M extends ReadOnly, T> {
    constructor() { }
}

class Sub<M extends ReadOnly> extends Super<M, Foo<M>> {
    constructor() {
        super();
    }
}

/*@ x :: Super<Immutable, Foo<Immutable>> */
let x = new Sub();
