/*@ type nat = {v: number | v >= 0 } */
type nat = number;

interface IPoint<M extends ReadOnly> {
    /*@ x : number */
    x: number;
    /*@ y : number */
    y: number;
}

interface INatPoint<M extends ReadOnly> {
    /*@ x : nat */
    x: number;
    /*@ y : nat */
    y: number;
}

interface IColorPoint<M extends ReadOnly> extends IPoint<M> {
    c: string;
}

class A<M extends ReadOnly> {
    /*@ a: nat */
    a: number = 1;

    /*@ foo(): void */
    private foo(): void { }

    /*@ bar(x: INatPoint<M>): void */
    bar(x: IColorPoint<M>): void { }

    constructor() { }
}
