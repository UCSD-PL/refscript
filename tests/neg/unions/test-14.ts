
declare function ofBoolean(x: boolean): void;

export function foo(x: number | boolean) {
    let tag = typeof x;
    if (tag === "number") {
        ofBoolean(<boolean>x);
    }
}
