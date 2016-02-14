class Packet<M extends ReadOnly> {
  /*@ (Mutable) link: Packet<Mutable> + undefined */
  public link: Packet<Mutable>;

  constructor() { }
}

/*@ foo :: (curr: Packet<Mutable>) => { Packet<Mutable> | 0 < 1 } */
function foo(curr) {
  let next = curr.link;

  if (next) curr = next;

  return next;
}
