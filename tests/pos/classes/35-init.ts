
class Foo<M extends ReadOnly> {
    public ffffff: number;

    constructor(a: number) {
        this.ffffff = a;
    }
}

export function createFoo(): Foo<Unique> {
    let foo = new Foo(5);
    foo.ffffff = 10;
    return foo;
}

let foo: Foo<Immutable> = createFoo();

// TODO: make the check redundant
if (foo.ffffff === 10) {
    assert(foo.ffffff === 10);
}
