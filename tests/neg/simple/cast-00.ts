
class HTMLElt<M extends ReadOnly> {
    constructor() { }
}

class HTMLDivElt<M extends ReadOnly> extends HTMLElt<M> {
    constructor() {
        super();
    }
}

class HTMLCanvasElt<M extends ReadOnly> extends HTMLElt<M> {
    constructor() {
        super();
    }
}

export function foo(elt: HTMLElt<Immutable>): HTMLDivElt<Immutable> {
    if (elt instanceof HTMLCanvasElt) {
        let div = <HTMLDivElt<Immutable>>elt;
        return div;
    }
    throw new Error("");
}
