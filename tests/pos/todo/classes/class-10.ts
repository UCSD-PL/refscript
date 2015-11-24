
/*@ foo :: (x: Node<Mutable> + undefined) => { void | 0 < 1 } */
function foo(x: Node) {
  if (x) {
    (<Node>x).field = 2;
  }
}

class Node {
  /*@ new () => Node<M> */
  constructor() { }

  /*@ field : [Mutable] number */
  public field = 0;
}

