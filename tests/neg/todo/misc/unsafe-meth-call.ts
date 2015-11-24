class Tree {
  /*@ new() => {void | 0 < 1} */
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  /*@ foo : () : {void | 0 < 1} */
  foo() {
    this.root.bar();
  }
}

declare class TreeNode {
  /*@ bar : () : {void | 0 < 1} */
  bar();
}

