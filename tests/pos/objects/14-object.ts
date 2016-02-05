
/*@ bar :: (a: (Mutable) { f: number }) => void */
function bar(a: { f: number }) {
    a.f = 1;
}

/*@ foo :: (x: (Mutable){ f: posint }) => void */
function foo(x: any) {
    bar(x);
    assert(x.f > 0);
}
