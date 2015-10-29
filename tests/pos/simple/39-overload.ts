
class FooT<M extends ReadOnly> {
    public fff<U>(a: (x: U) => U): Array<Immutable, U>;
    public fff<V>(a: (x: V, y: V) => V): Array<Immutable, V>;
    public fff(x: any): any {
        return [];
    }
    constructor() { }
}

function f1(x: number): number {
    return x;
}

function f2(x: posint, y: number): posint {
    return x;
}

export function foo(x: FooT<Immutable>): Array<Immutable, number> {
    let c = x.fff(f1);
    let b = x.fff(f2);
    return c;
}
