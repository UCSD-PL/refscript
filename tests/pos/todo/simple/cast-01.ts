

export function foo<V extends number | string>(x: V): V {
    return x;
}

foo(1);


// /*@ foo :: () => posint + undefined */
// declare function foo(): posint;
//
// export function bar(): number {
//     var x = foo();
//     if (x) {
//         return 1 + <number>x;
//     }
//     return 0;
// }
