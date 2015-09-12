
/*@ qualif PlusOne(v:number, x: number): v = x + 1 */
/*@ qualif PlusTwo(v:number, x: number): v = x + 2 */

/*@ readonly bar :: (x: number) => number */
var bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x: number) => number */
var foo = function(x: number) {
    var a = bar(x);
    var b = bar(a);
    return b;
}

var baz = foo;
assert(foo(1) === baz(2));
assert(bar(1) === bar(2));
