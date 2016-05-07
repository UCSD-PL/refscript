
function forloop<A>(lo: number, hi: number, body: (x: number, y: A) => A, accum: A): A {
    if (lo < hi) {
        let newAcc = body(lo, accum);
        return forloop(lo + 1, hi, body, newAcc);
    }
    return accum;
}

/*@ minIndex :: (a: { IArray<number> | 0 < len v }) => number */
export function minIndex(a: number[]): number {
    /*@ readonly */ let aa = a;
    let step: (i: number, min: number) => number = function(i, min) {
        if (aa[i] < aa[min]) {
            return i;
        }
        return min;
    }
    return forloop(0, aa.length, step, 0);
}
