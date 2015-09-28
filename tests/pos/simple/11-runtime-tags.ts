
export function foo(f: () => number): number;
export function foo(f: number): number
export function foo(f: any): number {
    if (typeof f === "function") {
        return f();
    }
    else {
        return f + 1;
    }
}
