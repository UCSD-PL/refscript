
interface Foo<M extends ReadOnly, T> { }

export declare function stringReduce(xf: Foo<Immutable, string>): void;

export function reduce(xf: Foo<Immutable, number>, coll: IArray<number>): void;
export function reduce(xf: Foo<Immutable, string>, coll: string): void;
export function reduce(xf: any, coll: any): void {
    if (typeof coll === "object") {
        stringReduce(xf);
    }
}
