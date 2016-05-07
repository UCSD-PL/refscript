
/*@ check_undefined :: <T>(x: T + undefined) => T */
export function check_undefined<T>(x: any): T {
    if (typeof x === "undefined")
        return crash();
    return x;
}
