class Packet {
  /*@ link : [Mutable] Packet<Mutable> + null */
  public link:Packet; 

  constructor() { }
}

/*@ foo :: (curr: Packet<Mutable>) => { Packet<Mutable> | true } + undefined */
function foo(curr) {
  let next = curr.link;
  /*@ curr1 :: Packet<Mutable> + undefined */
  let curr1 = curr;
  if (next) curr1 = next;  
  return curr1;
}
