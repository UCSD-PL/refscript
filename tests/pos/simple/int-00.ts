
/*@ foo :: ( x: number, y: boolean, z: string) => { number | 0 < 1 } */

function foo(x, y, z) {
    x = x + 1;
    return x;
}
