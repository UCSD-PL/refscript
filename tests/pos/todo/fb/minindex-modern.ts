
/*@ reduce :: <T, A>(arr:IArray<T>, callback: (x: A, y: T, idx<arr>) => A, init:A) => A */
function reduce(me, callback, init) {
    let res = init;
    for (let i = 0; i < me.length; i++) {
        res = callback(res, me[i], i);
    }
    return res;
}


/*@ minIndex :: (arrrr:IArray<number>) => {number | 0 < 1} */
function minIndex(arrrr) {

    /*@ readonly */
    let arr = arrrr;

    if (arr.length <= 0) return -1;

    let body: (acc: number, cur: number, i: number) => number = function(acc, cur, i) {
        return cur < arr[acc] ? i : acc;
    };

    return reduce(arr, body, 0);
}
