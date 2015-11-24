class Packet {
  /*@ link : [#Mutable] Packet<Mutable> + undefined */
  public link:Packet; 

  /*@ new() => void */
  constructor() { }
}

/*@ foo :: (curr: Packet<Mutable>) => { Packet<Mutable> | 0 < 1 } */
function foo(curr) {
  var next = curr.link;

  if (next) curr = next;
  
  return next;
}
