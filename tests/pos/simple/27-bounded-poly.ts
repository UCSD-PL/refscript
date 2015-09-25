
function foo<V extends number>(x: V, y: number, z: V): V {
    return x;
}

export function main() {
    assert(foo(2, 2, 2) === 2);
}
