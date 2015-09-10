/*@ foo :: (x: Node<Mutable> + undefined) => { void | 0 < 1} */
function foo(x: Node) {
  if (x) {
    x.field = 2;
  }
}
 
class Node {
  constructor() { }

  /*@ field : [Immutable] number */
  public field = 0;
}

