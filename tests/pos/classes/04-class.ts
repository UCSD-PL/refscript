
interface IPoint<M extends ReadOnly> {
    x: number;
    y: number;
}

interface ICPoint<M extends ReadOnly> {
    c: string;
}

class P<M extends ReadOnly> implements IPoint<M> {
    public x: number;
    public y: number;

    constructor(x: number) { this.x = x; this.y = x; }
}

class CP<M extends ReadOnly> extends P<M> implements ICPoint<M> {
    /*@ c : { string | v = "red" } */
    public c: string;

    /*@ new (c: { string | v = "red" }) => CP<M> */
    constructor(c: string) { super(1); this.c = c; }
}

class RCP<M extends ReadOnly> extends CP<M> {
    constructor() {
        super("red");
    }
}
