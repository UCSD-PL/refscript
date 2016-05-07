interface IPoint<M extends ReadOnly> {
    x: number;
    y: number;
}

interface INatPoint<M extends ReadOnly> {
    /*@ x : posint */
    x: number;
    /*@ y : posint */
    y: number;
}

interface IColorPoint<M extends ReadOnly> extends IPoint<M> {
    c: string;
}

class A<M extends ReadOnly> {
    /*@ a : { number | v > 0 } */
    a: number;

    private foo(): void { }
    bar(x: IColorPoint<M>): void { }
    constructor() {
        this.a = -2;
    }
}

class B<M extends ReadOnly> extends A<M> {
    /*@ a : number */
    a: number;

    bar(x: IPoint<M>): void { }
    constructor() { super(); }
}
