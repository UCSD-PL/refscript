/*@ foo :: (IArray<number>) => number */
function foo00(a: number[]): number {
    if (a.length > 0) {
        return a[0];
    }
    return 0;
}

foo00([]);
foo00([1]);
foo00([1, 2]);
foo00([1, 2, 3]);
foo00([1, 2, 3, 4]);
