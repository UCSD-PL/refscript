
/*@ bar :: (x: string) => { string | v = x } */
declare function bar(x: string): string;

function foo<V,U,W>(x: V, f: (v: U) => W, a: U): W {
    return f.call(x, a);
}


let a: IArray<string> = ["a"];

assert(foo(a, bar, "b") === "b");
