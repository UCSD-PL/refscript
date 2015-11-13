class Tree {
/*@ new() => {Tree<M> | 0 < 1} */
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  /*@ foo : () : {void | 0 < 1} */
  foo() {
    var _root = this.root;
    if (_root) {
      _root.bar();
    }
  }
}

declare class TreeNode {
  /*@ bar : () : {void | 0 < 1} */
  bar();
}

