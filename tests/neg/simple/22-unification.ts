
interface Pair<M extends ReadOnly, A, B> { x: A; y: B; }
export function fst<M extends ReadOnly, A, B>(p: Pair<M, A, B>): A { return p.x; }
export function snd<M extends ReadOnly, A, B>(p: Pair<M, A, B>): B { return p.y; }

/*@ neg_o_22_0 :: { @final z: string } */
let neg_o_22_0 = { z: "ASDFGHJKL" }
let neg_o_22_1 = snd({ x: 1, y: neg_o_22_0 });

assert(neg_o_22_1.z === "ASDFGHJK");
