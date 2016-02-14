/*@  qualif UBound(v: int, x:a) : (v < len x) */
/*@  qualif UBound(v: int) : (0 <= v) */

/*@ range :: (lo: number, hi: number) => IArray<number> */
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

// TODO: restore this type
/*  minIndex :: (a: { IArray<number> | 0 < len v }) => {v:number | (0 <= v && v < (len a )} */

/*@ minIndex :: (a: { IArray<number> | 0 < len v }) => number */
function minIndex(a) {

    /*@ readonly aa :: { IArray<number> | 0 < len v } */
    let ra = a;

    // TODO: lose the annotation
    /*@ step :: (i: idx<ra>, min: idx<ra>) => idx<ra> */
    function step(i: number, min: number): number {
        if (ra[i] < ra[min]) {
            return i;
        }
        return min;
    };

    let r = foldl(step, 0, range(0, ra.length));

    assert(r >= 0);
    assert(r < ra.length);
    return r;
}
