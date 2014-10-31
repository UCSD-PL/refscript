class Packet {
  /*@ link : [#Mutable] Packet<Mutable> + undefined */
  public link:Packet; 

  /*@ new() => void */
  constructor() { }
}

/*@ foo :: (curr: Packet<Mutable>) => { Packet<Mutable> | true } + undefined */
function foo(curr) {
  var next = curr.link;

  if (next) curr = next;
  
  return next;
}
