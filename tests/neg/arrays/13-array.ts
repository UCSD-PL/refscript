
export function revInc(a: MArray<number>): IArray<number> {
    a.reverse();
    for (let i = 0; i < a.length; i++) {
        a[i] = a[i] + 1;
    }
    return a;
}
