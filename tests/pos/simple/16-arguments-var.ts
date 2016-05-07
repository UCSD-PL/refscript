
export function foo() {
    /*@ bar :: (x:top) => number */
    /*@ bar :: ()      => number */
    let bar = function(x?) {
        if (arguments.length === 0)
            return 0;
        return 1;
    }
    return bar();
}
