
function reduce <T, A>(arr: IArray<T>, callback: (x: A, y: T, i: number) => A, init:A): A {
    let res = init;
    for (let i = 0; i < arr.length; i++) {
        res = callback(res, arr[i], i);
    }
    return res;
}

export function minIndex(a: IArray<number>): number {
    /*@ readonly */ let ro_a: IArray<number> = a;
    if (ro_a.length <= 0) return -1;
    let body: (acc: number, cur: number, i: number) => number = function(acc, cur, i) {
        return cur < ro_a[acc] ? i : acc;
    }
    return reduce(ro_a, body, 0);
}
