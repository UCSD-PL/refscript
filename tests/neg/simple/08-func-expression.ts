
/*@ readonly bar :: (x:number) => {number | v = x + 1} */
let bar = function(x: number) {
    return x + 1;
}

/*@ readonly foo :: (x:number) => {number | v = x + 2} */
let foo = function(x: number) {
    let a = bar(x);
    let b = bar(a);
    return b;
}

let baz = foo;
assert(foo(1) === baz(2));
