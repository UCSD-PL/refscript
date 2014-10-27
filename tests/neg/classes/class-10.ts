/*@ foo :: (x: Node<Mutable> + undefined) => { void | true } */
function foo(x: Node) {
  if (x) {
    x.field = 2;
  }
}

class Node {
  /*@ new () => void */
  constructor() { }

  /*@ field : [Immutable] number */
  public field = 0;
}

