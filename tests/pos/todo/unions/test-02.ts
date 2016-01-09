
/*@ check_undefined :: <T>(x: T + undefined) => T */
export function check_undefined<T>(x: any): T {
    if (typeof x === "undefined")
        return crash();

    // OK
    // return x;

    // FAILS
    return <T>x;
    // PROBABLY "parses" the above as "TApp T []" ... aha.
}
