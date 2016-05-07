/*@ foo :: ({ IArray<number> | len v >= 10 } ) => number */
export function foo(a: number[]): number {
    return a[9];
}

/*@ bar :: (a: IArray<number>, i: { number | 0 <= i && i+1 < len a}) => number */
export function bar(a: number[], i: number): number {
    return a[i + 1];
}
