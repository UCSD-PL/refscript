
/*@ qualif PlusOne(v:number, x: number): v = x + 1 */

/*@ readonly bar :: (x: number) => number */
var bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x: number) => number */
var foo = function(x: number): number {
    var a = bar(x);
    var b = bar(a);
    return b;
}

var baz = foo;
assert(bar(1) === 2);
// assert(foo(1) === baz(1));
