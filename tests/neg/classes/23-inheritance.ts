
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
    /*@ a : nat */
    a: number;

    private foo(): void {  }
    bar(x: IColorPoint<M>): void {  }
    baz(x: INatPoint<M>): void {  }

    constructor() {  }
}

class B<M extends ReadOnly> extends A<M> {
    constructor() { super(); }
}

class C<M extends ReadOnly> extends B<M> {
    /*@  a : nat */
    a: number;

    baz(x: INatPoint<M>): void {  }

    constructor() {
        super();
    }
}
