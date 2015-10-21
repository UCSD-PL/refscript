
/*@ qualif PlusOne(v:number, x: number): v = x + 1 */

/*@ readonly bar :: (x: number) => number */
let bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x: number) => number */
let foo = function(x: number): number {
    let a = bar(x);
    let b = bar(a);
    return b;
}

let baz = foo;
assert(bar(1) === 2);
// assert(foo(1) === baz(1));
