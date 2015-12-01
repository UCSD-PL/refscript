
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

let o   = { z: "ASDFGHJKL" }
let obj = snd({ x: 1, y: o });
let a   = obj.z;

assert(a === "ASDFGHJKL");
