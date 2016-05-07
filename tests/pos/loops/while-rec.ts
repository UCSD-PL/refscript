
/*@ qualif Ineq(v : int): (v <= 6) */
/*@ qualif Eq1(v: int): v = 6 */

function loop(x: number): number {
    if (x <= 5) {
        return loop(x + 1);
    }
    return x;
}

export function main(): void {
    let x = loop(0);
    assert(x === 6);
}
