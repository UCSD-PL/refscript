
/*@ qualif UBound(v: int, x:a) : v < (len x) */

/*@ minIndex :: (aa: IArray<number>) => {number | 0 < 1} */
function minIndex(aa: IArray<number>) {

    /*@ readonly a :: IArray<number>*/
    let a = aa;

    if (a.length <= 0) return -1;

    function body(acc: number, cur: number, i: number) {
        return cur < a[acc] ? i : acc;
    }

    return a.reduce(body, 0);
}
