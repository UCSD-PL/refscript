/*@ foo :: (x: number) => {number | v = x + 2} */
export function foo(x: number): number {
    let a = zogbert(x);
    let b = zogbert(a);
    return b;
}

/*@ zogbert :: (x: number) => {number | v = x + 1} */
function zogbert(x: number): number {
    return x + 1;
}
