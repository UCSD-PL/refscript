
/*@ bar :: (x: string) => { string | v = x } */
declare function bar(x: string): string;

function foo<V, U>(x: V, f: (v: U) => U, a: U): U {
    return f.call(x, a);
}

let a: IArray<string> = ["a"];

assert(foo(a, bar, "b") === "a");
