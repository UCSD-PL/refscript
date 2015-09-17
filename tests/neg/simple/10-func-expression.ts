
/*@ foo2 :: () => () => undefined */
export function foo2() {
    assert(false);
    return function() {
        return undefined;
    }
}
