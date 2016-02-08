/*@ qualif UBound(v: int, x:a) : v < (len x) */

/*@ loop :: (IArray<number>, number, number) => number */
function loop(b: number[], min: number, i: number): number {
    if (i < b.length) {
        let min_ = min;
        assert(i < b.length);
        if (b[i] < b[min]) {
            min_ = i;
        }
        return loop(b, min_, i + 1)
    }
    return min;
}

/*@ minIndex :: (a: { IArray<number> | 0 < len v }) => {v:number | (0 <= v && v < (len a))} */
function minIndex(a) {
    let r = loop(a, 0, 0);
    return r;
}
