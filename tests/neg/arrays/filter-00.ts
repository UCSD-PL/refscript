
/*@ array_filter :: <T>(IArray<T>, (x:T) => boolean) => IArray<T> */
export function array_filter(a, f) {
    return array_filter(a, f);
}

/*@ is_num :: (x: string) => { boolean | 0 < 1 } */
export function is_num(x: number): boolean {
    return !isNaN(x);
}

/*@ foo :: (IArray<number + undefined>) => IArray<number> */
export function foo(arr: any, f: any) {
    return array_filter(arr, is_num);
}
