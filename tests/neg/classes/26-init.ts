
class Foo<M extends ReadOnly> {
    public f: number;

    constructor(a: number) {
      this.f = a;
    }
}

function createFoo(): Foo<Immutable> {
    /*@ foo :: Foo<Unique> */
    let foo = new Foo(5);

    foo.f = 10;
    return foo;
}

let foo = createFoo();

if (foo.f === 10) {
    assert(foo.f === 100);
}
