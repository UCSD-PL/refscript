
//adapted from transducers

declare function bar(x: string): void;
declare function foo(x: {}): void;

export function reduce(x: string): void;
export function reduce(x: {}): void;
export function reduce(x: any): void {
    if (typeof x === "string") {
        bar(x);
    }
    else {
        foo(x);
    }
}
