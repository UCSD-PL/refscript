class Tree<M extends ReadOnly> {

    constructor() {}

    /*@ root : TreeNode<Immutable> + null */
    root = null;

    foo() {
        let _root = this.root;
        if (_root) {
            _root.bar();
        }
    }
}

class TreeNode<M extends ReadOnly> {
    bar(): void {} ;

    constructor() {}
}
