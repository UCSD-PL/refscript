//adapted from splay-typed-octane.ts
class ListNode {
    /*@ new () => {ListNode<M> | true} */
    constructor() { }
    /*@ right : ListNode<Immutable> + null */
    public right = null;
}

/*@ traverse_ :: (ListNode<Immutable>) => {void | true} */
function traverse_(x) {
    var current = x;
    while (current) {
        /*@ z :: ListNode<Immutable> */
        var z = current;
        current = current.right;
    }
}
