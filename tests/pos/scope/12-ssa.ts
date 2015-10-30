
/*@ qualif Cmp(v: number, n: number): v = 3 */

export function bar() {
    let x = 1;
    if (true) {
        x = 3;
    }
    assert(x === 3);

    let y = 1;
    if (true) {

    }
    assert(y === 1);
}
