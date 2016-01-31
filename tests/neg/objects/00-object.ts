
/*@ foo :: (x: posint) => { @Final a: { number | v > 1 } }  */
function foo(x) {
    return { a: x };
}
