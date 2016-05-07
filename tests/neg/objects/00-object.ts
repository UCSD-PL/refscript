
/*@ foo :: (x: posint) => { (Immutable) a: { number | v > 1 } }  */
function foo(x) {
    return { a: x };
}
