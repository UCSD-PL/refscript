//adapted from splay-typed-octane.ts

class ListNode<M extends ReadOnly> {
    constructor() { }

    /*@ right : ListNode<Immutable> + null */
    public right = null;
}

export function traverse_(x: ListNode<Immutable>) {

    /*@ local current :: ListNode<Immutable> + null */
    let current = x;

    while (current) {
        let z = <ListNode<Immutable>>current;
        current = current.right;
    }
}
