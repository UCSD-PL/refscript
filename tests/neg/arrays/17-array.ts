
/*@ bar :: (IArray<number>) => void */
function bar(a) {
    a[0] = -1;
}

/*@ foo :: ({ IArray<posint> | len v > 0 }) => void */
function foo(a) {
    bar(a);
    assert(a[0] > 0);
}
