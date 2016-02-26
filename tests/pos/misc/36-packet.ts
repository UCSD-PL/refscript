class Packet<M extends ReadOnly> {
  /*@ (Mutable) link: Packet<Mutable> + null */
  public link: Packet<Mutable> = null;

  constructor() { }
}

/*@ foo :: (curr: Packet<Mutable>) => Packet<Mutable> + undefined */
export function foo(curr) {
    let next = curr.link;
    /*@ local curr1 :: Packet<Mutable> + undefined */
    let curr1 = curr;
    if (next)
        curr1 = next;
    return curr1;
}
