
export function foo(x: number | boolean): number | string {
    if (typeof x === "number")
        return x;
    return "a";
}
