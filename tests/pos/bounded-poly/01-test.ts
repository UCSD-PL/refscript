

export function foo<V extends number | string>(x: V): V {
    return x;
}

assert(foo(1) === 1);
