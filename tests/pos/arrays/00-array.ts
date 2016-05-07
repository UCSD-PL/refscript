
export function foo(a: IArray<number>) {
    if (a.length > 0) {
        return a[0];
    }
    return 0;
}

foo(<IArray<number>>[]);
foo(<IArray<number>>[1]);
foo(<IArray<number>>[1, 2]);
foo(<IArray<number>>[1, 2, 3]);
foo(<IArray<number>>[1, 2, 3, 4]);
