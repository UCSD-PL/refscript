
class Point<M extends ReadOnly> {
    public x : number = 1;
    public y : number = 2;
    constructor() { }
}

class ColorPoint<M extends ReadOnly> extends Point<M> {
    public c : string = "";
    constructor() { super(); }
}

class RedPoint<M extends ReadOnly> extends ColorPoint<M> {
    /*@ c : { string | v = "red" } */
    public c : string = "red";
    constructor() {
        super();
    }
}

/*@ a :: { v: { x: number; y: number } | extends_class(v, "Point") } */
let a: Point<Immutable> = new Point<Immutable>();

assert(a instanceof Point);
