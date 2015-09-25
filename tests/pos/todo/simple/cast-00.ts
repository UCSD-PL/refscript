
class HTMLElt<M extends ReadOnly> {
    constructor() {

    }
}

class HTMLDivElt<M extends ReadOnly> extends HTMLElt<M> {
    constructor() {
        super();
    }
}

export function foo<M extends ReadOnly>(elt: HTMLElt<M>): HTMLDivElt<M> {
    if (elt instanceof HTMLDivElt) {
        var div = <HTMLDivElt<M>>elt;
        return div;
    }

    // return undefined; // new HTMLDivElt<Mutable>();
    throw new Error("");
}
