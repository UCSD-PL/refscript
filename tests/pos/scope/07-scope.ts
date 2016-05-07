//adapted from underscore
export class Foo<M extends ReadOnly> {
    constructor() { }

    map<T>(): void {
        /*@ results :: Array<Mutable, T> */
        let results = [];
    }
}

// Note that it works outside of a class context:

export function map<T>(): void {
    /*@ results :: Array<Mutable, T> */
    let results = [];
}
