
/*@ foo :: (x: Node<Mutable> + undefined) => void */
export function foo(x: Node<Mutable>) {
  if (x) {
    (<Node<Mutable>>x).field = 2;
  }
}

class Node<M extends ReadOnly> {
  constructor() { }

  /*@ (Mutable) field: number */
  public field = 0;
}
