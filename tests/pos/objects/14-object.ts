
function bar(a: { f: number }) {
    a.f = 1;
}

/*@ foo :: (x: { f: posint }) => void */
function foo(x: any) {
    bar(x);
    assert(x.f > 0);
}
