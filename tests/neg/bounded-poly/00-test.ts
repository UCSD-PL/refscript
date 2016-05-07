

export function foo<V extends number>(x: V): V;
export function foo<V extends string>(x: V): V;
export function foo(x: any): any {
    return x;
}

let a = foo("");


assert(typeof foo(1) === "string");
assert(typeof foo("") === "number");
