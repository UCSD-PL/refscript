

export function foo<V extends number | string>(x: V): V {
    return x;
}

assert(typeof foo(1) === "string");
assert(typeof foo("") === "number");
