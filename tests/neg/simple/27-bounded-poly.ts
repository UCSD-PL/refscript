
/*@ foo :: <V extends posint>(x: V) => V */
function foo<V extends number>(x: V): V {
    return x;
}

export function main() {
    assert(foo(-2) === -2);
}
