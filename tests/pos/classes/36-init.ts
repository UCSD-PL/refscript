
class Foo<M extends ReadOnly> {

    /*@ f : { number | v = 4 } */
    public f: number;

    /*@ new (id: { number | v = 4 }): Foo<M> */
    constructor(a: number) {
        this.f = a;
    }
}

export function createFoo(): Foo<Immutable> {
    let foo = new Foo(4);
    foo.f = 4;
    return foo;
}
