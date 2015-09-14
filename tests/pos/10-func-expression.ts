
/*@ foo :: (x: number) => () => undefined */
export function foo(x: number) {
    /*@ readonly f :: () => undefined */
    var f = function() {
        return undefined;
    }
    return f;
}
