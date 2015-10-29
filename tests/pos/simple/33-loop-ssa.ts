//adapted from splay-typed-octane.ts

class ListNode<M extends ReadOnly> {

    constructor() { }

    /*@ right : ListNode<Immutable> + null */
    public right = null;

}

/*@ traverse_ :: (ListNode<Immutable>) => {void | true} */
function traverse_(x) {

    /*@ local current :: ListNode<Immutable> + null */
    let current = x;

    while (current) {
        let z = <ListNode<Immutable>>current;
        current = current.right;
    }

}
