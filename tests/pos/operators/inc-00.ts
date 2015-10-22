/*@ qualif PlusOne(v:number, x:number)   : v = x + 1    */

function inc(x: number) {
    return x + 1;
}

export function main() {
    let a = pos();
    let b = inc(a);
    assert(b === a + 1);
    assert(b > 0);
}
