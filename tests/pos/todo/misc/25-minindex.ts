/*@  qualif UBound(v: int, x:a) : (v < len x) */

/*@ range :: (number, number) => IArray<number> */
function range(lo: number, hi: number) {
    if (lo < hi) {
        let rest: number[] = range(lo + 1, hi);
        let loa: IArray<number> = [lo];
        return loa.concat(rest);
    }

    return [];
}

/*@ foldl :: <A, B>((A, B) => A, A, IArray<B>) => A */
function foldl(f, acc, xs) {
    if (xs.length <= 0) {
        return acc;
    } else {
        assert(xs.length > 0);
        let acc_ = f(acc, xs[0]);
        return foldl(f, acc_, xs.slice(1, xs.length));
    }
}

/*@ minIndex :: (a: { IArray<number> | 0 < len v }) => {v:number | (0 <= v && v < (len a)  )} */
function minIndex(a) {

    /*@ readonly aa :: { IArray<number> | 0 < len v } */
    let ra = a;

    function step(i: number, min: number) {
        if (ra[i] < ra[min]) {
            return i;
        }
        return min;
    };

    return foldl(step, 0, range(0, a.length));
}
