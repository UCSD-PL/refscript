

class Foo<M extends ReadOnly> {
    public x:number;

    constructor(x?: number) {
        if (x) this.x = x;
        else   this.x = 0;
    }
}

let f = new Foo(1);
let h = new Foo();
