
interface A<M extends ReadOnly, T> {
    f: T;
}


interface C<M extends ReadOnly, T> extends A<M, number> {
    minChar: number;
    limChar: number;
    trailingTriviaWidth: number;
    g: T;
}
