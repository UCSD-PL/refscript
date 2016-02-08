class Foo<M extends ReadOnly> {
    public x: number;
    constructor() {
        this.x = 3;
    }

    /*@ @Mutable bar(): void */
    bar() {
        this.x = 4;
    }
}

let f = new Foo<Mutable>();
f.bar();
