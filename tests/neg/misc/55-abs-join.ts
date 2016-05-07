
export function abs(x: number): number {
    let res: number | boolean = 0;
    if (x > 0) {
        res = x;
    } else {
        res = (x > 99);
    };

    assert(res >= 0);
    return <number>res;
}
