/*@ abs :: ( f: ( number: top ) => number, x: number ) => number */
function abs(f, x) {
    var r = x;
    if (x < 0) {
        r = 0 - x;
    }
    r = f(r);
    assert(r >= 0);
    return r;
}


/*@ dubble :: ( p: number ) => number */
function dubble(p) {
    return p + p;
}


/*@ main :: ( y: number ) => number */
function main(y) {
    var yy = abs(dubble, y);
    assert(yy >= 0);
    return yy;
}
