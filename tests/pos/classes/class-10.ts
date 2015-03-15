
/*@ foo :: (x: Node<Mutable> + undefined) => { void | true } */
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

