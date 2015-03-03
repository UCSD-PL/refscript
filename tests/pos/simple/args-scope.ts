//adapted from d3 (bisect)
function foo() {
    var bar = function(x?) 
    /*@ <anonymous> /\ (x:top) => number
                    /\ ()      => number */
    {
        if (arguments.length === 0) return 0;
        return 1;
    }
    return bar;
}
