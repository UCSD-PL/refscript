
/*@ qualif Plus(v: int, x: int, y: int): v = x + y    */

function sumLoop(acc: number, i: number): number {
    let r: number = 0;
    if (0 < i) {
        r = sumLoop(acc + 1, i - 1);
    } else {
        r = acc;
    }
    return r;
}

export function main() {
    let n: number = pos();
    let m: number = sumLoop(0, n);
    assert(m === n);
}
