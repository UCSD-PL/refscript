/*@ bog :: (xag: number) => {number | v = xag } */
/*@ bog :: (xag: { string | false } ) => string */
export function bog(x) {
    return x;
}

/*@ gloop :: (number) => number */
export function gloop(y: number): number {
    let z: number = bog(y);
    return z;
}
