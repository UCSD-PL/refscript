
class Foo<M extends ReadOnly, A> {
    public f: A;

    constructor(x: A) {
        this.f = x;
    }
}

let p: number = 0;

let a: Foo<Immutable, number> = new Foo(p);

assert(a.f > 0);
