class Tree {
/*@ new() => {Tree<M> | true} */
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  /*@ foo : () : {void | true} */
  foo() {
    let _root = this.root;
    if (_root) {
      _root.bar();
    }
  }
}

declare class TreeNode {
  /*@ bar : () : {void | true} */
  bar();
}

