class Tree {
  /*@ new() => {void | true} */
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  /*@ foo : () : {void | true} */
  foo() {
    var _root = this.root;
    if (_root) {
      _root.bar();
    }
  }
}

declare class TreeNode {
  /*@ bar : () : {void | true} */
  bar();
}

