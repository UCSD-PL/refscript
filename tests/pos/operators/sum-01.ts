
/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc: number, i: number): number {
    if (0 < i) {
        return sumLoop(acc + 1, i - 1);
    }
    return acc;
}

export function main(): void {
    let n: number = pos();
    let m: number = sumLoop(0, n);
    assert(m === n);
}
