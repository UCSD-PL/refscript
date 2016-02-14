
/*@ foo :: <T>(x: IArray<T>) => T + undefined */
export function foo<T>(x: IArray<T>): T {
    /*@ b :: T + undefined */
    let b: T;
    if (x.length > 0) {
        b = x[0];
    }
    return b;
}
