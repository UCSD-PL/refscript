
interface Pair<M extends ReadOnly, A, B> {
    x: A;
    y: B;
}

export function fst<M extends ReadOnly, A, B>(p: Pair<M, A, B>): A {
    return p.x;
}

export function snd<M extends ReadOnly, A, B>(p: Pair<M, A, B>): B {
    return p.y;
}

var o = { z: "ASDFGHJKL" }

var obj = snd({ x: 1, y: o });

var a = obj.z;

assert(a === "ASDFGHJK");
