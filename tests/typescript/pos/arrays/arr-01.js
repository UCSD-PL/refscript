/*@ foo :: ({ [number] | (len v) = 1 } ) => { number | true } */
function foo(a) {
    return a[0];
}
