/*@ id2 :: <A,B>(A, B) => A */
function id2(x, y) {
    return x;
}

/*@ main :: (x:number, boolean) => {v:number|v >= x} */
function main(x, y) {
    let yr = id2(y, x);
    let xr = id2(x, y);
    let z = 0;
    if (yr) {
        z = 10;
    }
    return xr + z;
}
