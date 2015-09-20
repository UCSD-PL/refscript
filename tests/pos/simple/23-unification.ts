
function getX<A>(p: { x: A }): A {
  return p.x;
}

var a = getX({ x: 1 });

assert(a === 1);
