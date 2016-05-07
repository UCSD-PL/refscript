
/*@ abs :: ((number) => number, number) => number */
function abs(f, x) {
    let r = x;
    if (x < 0) {
        r = 0 - x;
    }
    r = f(r);
    assert(r >= 0);
    return r;
}

/*@ dubble :: (p: {v:number | v >= 0}) => { v: number | v >= p } */
function dubble(p) { return p + p }

/*@ main :: (y: number) => {v:number | v >= 0 } */
function main(y) {
    let y1 = abs(dubble, y);
    assert(y1 >= 0);
    return y1;
}

// p>=0  <: K?
// v>=p  <: K4
//
// -----------------------------
// p>=0 => v >= p  <:   K? => K4
