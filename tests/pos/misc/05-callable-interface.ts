
interface Foo<M extends ReadOnly> {
    (): number;
    (x: string): string;
}

declare let a: Foo<Immutable>;

assert(typeof a() === "number");
assert(typeof a("aa") === "string");
