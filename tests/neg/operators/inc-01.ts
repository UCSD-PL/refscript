/*@ qualif PlusOne(v: int,x: int): v = x + 1 */

function inc(x: number) {
    let res = x + 1;
    return res;
}

export function main(): void {
    let a = pos();
    let b = inc(a);
    assert (b > (a + 1));
    assert(b > 0);
}
