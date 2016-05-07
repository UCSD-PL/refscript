
interface A<M extends ReadOnly, T> {
    f: T;

    /*@ g : { number | v > 0 } */
    g: number;
}

/*@ interface C<M extends ReadOnly, T> extends A<M, { number | v < 0 } > */
interface C<M extends ReadOnly, T> extends A<M, number> {
    minChar: number;
    limChar: number;
    trailingTriviaWidth: number;
}
