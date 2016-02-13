/*@ option --extrainvs */


class Point<M extends ReadOnly> {
  x: number = 1;
  y: number = 2;
  constructor () {}
}

assert("x" in new Point());  // returns true
assert("z" in new Point());  // returns false
