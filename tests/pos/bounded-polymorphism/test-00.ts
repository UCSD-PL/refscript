

export function foo<V extends number | string>(x: V): V {
    return x;
}

let a_18_0 = foo(1);
let a_18_1 = foo("");

assert(typeof foo(a_18_0) === "number");
assert(typeof foo(a_18_1) === "string");
