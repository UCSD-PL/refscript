
/*@ readonly bar :: (x:number) => {number | v = x + 1} */
var bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x:number) => {number | v = x + 2} */
var foo = function(x: number) {
    var a = bar(x);
    var b = bar(a);
    return b;
}

var baz = foo;
assert(foo(1) === baz(2));
