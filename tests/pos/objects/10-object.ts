/*@ foo:: (o: { (Immutable) x: number; }) => { (Immutable) x: number + boolean } */
export function foo(o) {
    return o;
}
