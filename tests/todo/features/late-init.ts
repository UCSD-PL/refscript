/*@ foo :: (x:Node<Mutable>, y:Node<Mutable> + null) => { void | true } */
function foo(x, y) {
  var right, noOp;
  /*@ current :: Node<Mutable> */
  var current = x;
  if (!y) {
    noOp = 0;
  } else {
    right = current;
    current = y;
  }

  var a = right;

}

class Node {
  constructor() { }
}
