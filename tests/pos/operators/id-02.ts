/*@ idt :: <A>(A) => A */
function idt(x) { return x; }


/*@ idbool :: (boolean) => boolean */
function idbool(x) { return idt(x); }

/*@ main :: (x:number, boolean) => {v:number|v = x} */
export function main(x, y) {
    let yr = idt(y);
    let xr = idt(x);
    let z = 0;
    if (yr) {
        z = 10;
        return xr;
    }
    return xr + z;
}
