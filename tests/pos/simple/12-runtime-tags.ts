
export function foo(f: () => number): number;
export function foo(f: number): number
export function foo(f: any): number {
    return (typeof f === "function") ? f() : f;
}
