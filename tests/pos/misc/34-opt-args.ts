
/*@ foo :: () => {void | true} */
/*@ foo :: (l: {number | 0 <= v}) => {void | true} */
function foo(l?: number) {
    /*@ local l1 :: number */
    let l1 = (arguments.length < 1) ? 0 : l;
    assert(0 <= l1);
}
