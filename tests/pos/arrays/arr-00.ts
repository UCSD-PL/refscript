
function foo(a: IArray<number>): number {
    if (a.length > 0) {
        return a[0];
    }
    return 0;
}

foo([]);
foo([1]);
foo([1, 2]);
foo([1, 2, 3]);
foo([1, 2, 3, 4]);
