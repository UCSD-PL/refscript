/*@ index :: (a: IArray<number>, i: {number|(0 <= v && v < (len a))}) => number */
export function index(a: number[], i: number): number {
    return a[i];
}
