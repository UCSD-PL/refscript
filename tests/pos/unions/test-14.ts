
declare function ofNumber(x: number): void;

export function foo(x: number | boolean) {
    if (typeof x === "number") {
        ofNumber(x);
    }
}
