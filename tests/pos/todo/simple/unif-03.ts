
interface Pair1<M extends ReadOnly, A> {
    x: A;
    y: A;
}

interface ColorPair1<M extends ReadOnly, A, C> extends Pair1<M, A> {
    c: C;
}

export function foo<M extends ReadOnly, A>(p: Pair1<M, A>) {

}

declare var p: ColorPair1<Immutable, number, string>;

foo(p);

assert(typeof p.x === "number");
