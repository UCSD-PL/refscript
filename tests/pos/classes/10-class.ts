class Foo<M extends ReadOnly>{
    constructor() { }
}

export declare function bar(): void;

export function foo(f: Foo<Mutable>): () => void;
export function foo(f: () => void): () => void;
export function foo(f: any): () => void {
    return (typeof f === "function") ? f : bar
}
