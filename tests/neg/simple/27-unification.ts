
export declare function bar(): boolean;

export function foo<A>(x: A): A {
    let a = undefined;
    if (bar()) {
        a = x;
    }
    return a;
}
