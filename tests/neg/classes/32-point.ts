
class Point<M extends ReadOnly> {
    public x : number;
    public y : number;
    constructor() {}
}

class ColorPoint<M extends ReadOnly> extends Point<M> {
    public c : string;
    constructor() {
        super();
    }
}

class RedPoint<M extends ReadOnly> extends ColorPoint<M> {
    /*@ c : { string | v = "red" } */
    public c : string = "red";
    constructor() {
        super();
    }
}

let a: RedPoint<Mutable> = { x: 1, y: 2, c: "blue" };
