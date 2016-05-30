
class HTMLElt<M extends ReadOnly> {
    constructor() { }
}

class DivElt<M extends ReadOnly> extends HTMLElt<M> {
    public htmlDivElt__ = 0;
    constructor() {
        super();
    }
}

class SpanElt<M extends ReadOnly> extends HTMLElt<M> {
    public htmlSpanElt__ = 1;
    constructor() {
        super();
    }
}

class CanvasElt<M extends ReadOnly> extends HTMLElt<M> {
    public htmlCanvasElt__ = 2;
    constructor() {
        super();
    }
}

/*@ predicate TT v x n e = (x = n => extends_class(v, e)) */

/*@ createElt :: (tagName: string) => {v: HTMLElt<ReadOnly> |
    (TT v tagName "div" "DivElt") &&
    (TT v tagName "span" "SpanElt") &&
    (TT v tagName "canvas" "CanvasElt")
} */
export function createElt(tagName: string): HTMLElt<ReadOnly> {

    if (tagName === "div")
        return <HTMLElt<ReadOnly>>new DivElt();
    else if (tagName === "span")
        return <HTMLElt<ReadOnly>>new SpanElt();
    else if (tagName === "canvas")
        return <HTMLElt<ReadOnly>>new CanvasElt();
    else
        return <HTMLElt<ReadOnly>>new HTMLElt();
}

let elt = createElt("div"); // Try changing "div"

assert(elt instanceof DivElt);
