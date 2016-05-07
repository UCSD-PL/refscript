
interface Pair1<M extends ReadOnly, A> {
    x: A;
    y: A;
}

interface ColorPair1<M extends ReadOnly, A, B> extends Pair1<M, A> {
    b: B;
}

export function foo<M extends ReadOnly, A>(p: Pair1<M, A>) { }

declare let p: ColorPair1<Immutable, number, string>;

foo(p);

assert(typeof p.x === "string");
