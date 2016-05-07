/*@ qualif PlusOne(v: int, x: int)   : v = x + 1    */

function inc(x: number) {
    return x + 1;
}

export function main() {
    let a = _pos();
    let b = inc(a);
    assert(b === a + 1);
    assert(b > 0);
}
