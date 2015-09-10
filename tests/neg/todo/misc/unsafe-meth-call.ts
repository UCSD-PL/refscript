class Tree {
  /*@ new() => {void | true} */
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  /*@ foo : () : {void | true} */
  foo() {
    this.root.bar();
  }
}

declare class TreeNode {
  /*@ bar : () : {void | true} */
  bar();
}

