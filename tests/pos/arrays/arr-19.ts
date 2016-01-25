
/*@ qualif Length(v: a): len v > 0 */

function bar(a: IArray<number>) {
    a[0] = 1;
}

/*@ foo :: ({ IArray<{ number | v > 0 }> | (len v) > 0 }) => void */
function foo(a) {
    bar(a);
    assert(a[0] > 0);
}
