
/*@ foo :: (x: number) => () => undefined */
export function foo(x: number) {
    /*@ readonly f :: () => undefined */
    let f = function() {
        return undefined;
    }
    return f;
}
