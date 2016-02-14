class Tree<M extends ReadOnly> {
  constructor() {}

  /*@ root : TreeNode<Immutable> + null */
  root = null;

  foo(): void {
    this.root.bar();
  }
}

class TreeNode<M extends ReadOnly> {
    bar(): void {};
    constructor() {}

}
