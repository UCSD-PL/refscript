/*@ foo :: (x: Node<Mutable> + undefined) => void */
export function foo(x: Node<Mutable>) {
    if (x) {
        x.field = 2;
    }
}

class Node<M extends ReadOnly> {
    constructor() { }

    /*@ (Immutable) field: number */
    public field = 0;
}
