/*@ foo:: (o: { @Final x: number; }) => { @Final x: number + boolean } */
export function foo(o) {
    return o;
}
