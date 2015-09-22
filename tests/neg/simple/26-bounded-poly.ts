

function foo<V extends number>(x: number): number {
    return x;
}

export function main() {
    assert(foo(2)
    ===
     3);
}
