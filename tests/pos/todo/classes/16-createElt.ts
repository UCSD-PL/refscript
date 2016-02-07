
type HTML = HTMLElt<Mutable>

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

//function createElt(tagName: "div"): HTMLDivElt;
//function createElt(tagName: "span"): HTMLSpanElt;
//function createElt(tagName: "canvas"): HTMLCanvasElt;

/*@ predicate TT v x n e = (x = n => extends_class(v, e)) */

/*@ createElt :: (tagName: string) => {v: HTML |  TT v tagName "div" "DivElt"
                                               && TT v tagName "span" "SpanElt"
                                               && TT v tagName "canvas" "CanvasElt" } */

export function createElt(tagName: string): HTMLElt<ReadOnly> {

    /*@ local res :: HTML + undefined */
    let res: HTMLElt<ReadOnly>;

    if (tagName === "div") res = <HTMLElt<ReadOnly>>new DivElt();
    else if (tagName === "span") res = <HTMLElt<ReadOnly>>new SpanElt();
    else if (tagName === "canvas") res = <HTMLElt<ReadOnly>>new CanvasElt();
    else res = <HTMLElt<ReadOnly>>new HTMLElt();
    return res;
}

let elt = createElt("div");

assert(elt instanceof DivElt);
