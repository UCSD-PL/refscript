
function range(lo: number, hi: number): IArray<number> {
    if (lo < hi) {
        let rest: number[] = range(lo + 1, hi);
        let loa: IArray<number> = [lo];
        return loa.concat(rest);
    }
    return [];
}

function foldl<A, B>(f: (x: A, y: B) => A, acc: A, xs: IArray<B>): A {
    if (xs.length <= 0) {
        return acc;
    } else {
        assert(xs.length > 0);
        let acc_ = f(acc, xs[0]);
        return foldl(f, acc_, xs.slice(1, xs.length));
    }
}

// TODO: restore this type -- see comments below
/*  minIndex :: (zzzzz: { IArray<number> | 0 < len v }) => {v:number | (0 <= v) && (v < len a )} */

/*@ minIndex :: (a: { IArray<number> | 0 < len v }) => number */
function minIndex(zzzzz) {

    // RJ: why?
    // PV: cause ATM `a` is a local variable and so can be updated within `minIndex`.
    //     To avoid having it in refinements we disallow non-readonly vars. We
    //     can add an annotation to make `a` readonly.


    /*@ readonly aa :: { IArray<number> | 0 < len v } */
    let ra = zzzzz;

    let step: (i: number, min: number) => number = function (i, min){
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
