/*@ foo :: ({[number] | (len v) > 0 }) => { number | true } */
/* foo :: (number) => { number | true } */
function foo(a) {
    return a[0];
}
