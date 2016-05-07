
export declare function bar(): boolean;

export function foo<A>(x: A, y: A): A {
    let a = undefined;
    if (bar()) {
        a = x;
    }
    else {
        a = y;
    }
    return a;
}
