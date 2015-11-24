class SplayTreeNode {

  /*@ right : [Mutable] SplayTreeNode<Mutable> + undefined */
  public right;

  /*@ traverse_ : (this: SplayTreeNode<Mutable>) : { void | 0 < 1 } */
  public traverse_() {
    var current = this;
    while (current) {
      var peek = <SplayTreeNode>current;
      current = peek.right;
    }
  }
}
