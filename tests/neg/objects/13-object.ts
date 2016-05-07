
/*@ foo :: ( (Mutable){x: { v: number | v > 10 } }) => void */
export function foo(o) {
    o.x = 5;
}
