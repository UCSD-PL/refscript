
export function foo() {
    /*@ bar :: (x:top) => number */
    /*@ bar :: ()      => number */
    var bar = function(x?) {
        if (arguments.length === 0)
            return 0;
        return 1;
    }
    return bar();
}
