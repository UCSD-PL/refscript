
/*@ qualif Plus(v: int, x: int, y:int): v = x + y */

function sumLoop(acc: number, i: number): number {
    if (0 < i) {
        return sumLoop(acc + 1, i - 1);
    }
    return acc;
}

export function main(): void {
    let n: number = _pos();
    let m: number = sumLoop(0, n);
    assert(m === n);
}
