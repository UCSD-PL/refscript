/*@ foo:: (o: { (Mutable) x: number; }) => { (Mutable) x: number + boolean } */
export function foo(o: { x: number; }): { x: number | boolean } {
    return o;
}
