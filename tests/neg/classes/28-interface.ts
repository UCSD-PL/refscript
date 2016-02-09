
interface Point<M extends ReadOnly> {
    x: number;
    y: number;
}

interface ColorPoint<M extends ReadOnly> extends Point<M> {
    c: string;
}

interface RedPoint<M extends ReadOnly> extends ColorPoint<M> {
    /*@ c : { string | v = "red" } */
    c: string;
}

let a: RedPoint<Mutable> = { x: 1, y: 2, c: "blue" };
