
declare function bar(x: string): void;

function foo<V, U>(x: V, f: (v: U) => U, a: U): U {
    return f.call(x, a);
}

foo(["a"], bar, "b");
