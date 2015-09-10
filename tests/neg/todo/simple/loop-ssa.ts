//adapted from splay-typed-octane.ts
class ListNode {
    /*@ new () => {ListNode<M> | 0 < 1 } */
    constructor() { }
    /*@ right : ListNode<Immutable> + null */
    public right = null;
}


/*@ traverse_ :: (ListNode<Immutable>) => {void | 0 < 1} */
function traverse_(x) {

    /*@ local current :: ListNode<Immutable> + null */
    var current = x;

    while (current) {
        /*  z :: ListNode<Immutable> */
          var z = <ListNode>current;

        current = current.right;

        current = current.right;
    }
}
