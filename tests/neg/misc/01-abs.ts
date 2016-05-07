export function abs(x: number): number {
    let y = x;
    if (x > 0) {
        y = x;
    } else {
        y = 10 + x;
    };
    assert(y > 10);
    assert(y >= 100);
    return y;
}
