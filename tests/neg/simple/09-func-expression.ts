
/*@ qualif PlusOne(v:number, x: number): v = x + 1 */
/*@ qualif PlusTwo(v:number, x: number): v = x + 2 */

/*@ readonly bar :: (x: number) => number */
let bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x: number) => number */
let foo = function(x: number) {
    let a = bar(x);
    let b = bar(a);
    return b;
}

let baz = foo;
assert(foo(1) === baz(2));
assert(bar(1) === bar(2));
