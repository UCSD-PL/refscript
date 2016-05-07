
/*@ check_undefined :: <T>(T + undefined) => T */
function check_undefined<T>(x: T): T {
    if (typeof x === "undefined")
        return crash();
    return x;
}

/*@ bob :: (number) => number + undefined */
function bob(x: number): number {
    if (x > 0)
        return x;
    return undefined;
}

export function bar(x: number): number {
    let z = bob(x);
    let r = check_undefined(z);
    return r;
}
