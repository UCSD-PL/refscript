//adapted from splay-typed-octane.ts

interface ListNode<M extends ReadOnly> {
    /*@ right : ListNode<Immutable> + null */
    right: ListNode<Immutable>;
}

/*@ traverse_ :: (x: ListNode<Immutable> + null) => void */
export function traverse_(x: ListNode<Immutable>) {

    /*@ local current :: ListNode<Immutable> + null */
    let current = x;

    while (current) {
        let z = <ListNode<Immutable>>current;
        current = current.right;
        current = current.right;
    }

}
