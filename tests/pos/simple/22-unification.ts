
interface Pair<M extends ReadOnly, A, B> { x: A; y: B; }
export function fst<M extends ReadOnly, A, B>(p: Pair<M, A, B>): A { return p.x; }
export function snd<M extends ReadOnly, A, B>(p: Pair<M, A, B>): B { return p.y; }

/*@ o_22_0 :: { @Final z: { string | v = "ASDFGHJKL" } } */
let o_22_0 = { z: "ASDFGHJKL" }
let o_22_1 = snd({ x: 1, y: o_22_0 });

assert(o_22_0.z === "ASDFGHJKL");
assert(o_22_1.z === "ASDFGHJKL");
