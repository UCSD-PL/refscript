//adapted from transducers

interface Foo<M extends ReadOnly> { y: number }

interface Bar<M extends ReadOnly> { x: number }

class FooImpl<M extends ReadOnly> implements Foo<M>, Bar<M> {

    x = 1;
    y = 2;
    constructor() { }
}

/*@ x :: Foo<Immutable> */
let x = new FooImpl<Immutable>();

/*@ y :: Bar<Immutable> */
let y = new FooImpl<Immutable>();
