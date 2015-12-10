/*@ bog :: (x: number) => { number | v = x } */
/*@ bog :: (x: { string | false } ) => string */
export function bog(x) {
    return x;
}

export function gloop(y: number): number {
    let z: number = bog(y);
    return z;
}
