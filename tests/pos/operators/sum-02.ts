
/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

function sumLoop(acc: number, i: number): number {
    let r: number = acc;
    if (0 < i) {
        r = sumLoop(acc + 1, i - 1);
    }
    return r;
}

export function main(): void {
    let n: number = pos();
    let m: number = sumLoop(0, n);
    assert(m === n);
}
