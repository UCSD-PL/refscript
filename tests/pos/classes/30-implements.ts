//adapted from transducers

interface Foo<M extends ReadOnly> {}

class FooImpl<M extends ReadOnly> implements Foo<M> {
    constructor() { }
}

/*@ x :: Foo<Immutable> */
let x = new FooImpl();
