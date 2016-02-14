class Foo<M extends ReadOnly> {
    /*@ new (x: number, y: {number | v < x}): Foo<M> */
    constructor(x, y) { }
}
