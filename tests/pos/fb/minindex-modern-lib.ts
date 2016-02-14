
/*@ qualif UBound(v: int, x:a) : v < (len x) */

export function minIndex(aa: IArray<number>): number {
    /*@ readonly a :: IArray<number>*/
    let aaaaaa = aa;

    if (aaaaaa.length <= 0)
        return -1;

    let body: (acc: number, cur: number, i: number) => number = function (acc, cur, i) {
        return cur < aaaaaa[acc] ? i : acc;
    }

    return aaaaaa.reduce(body, 0);


}
