/*@ option --extrainvs */

/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnumP (p: Str, x: a): enumProp x p */

/*@ qualif HasP (x: a, p: Str): hasProperty x p */
/*@ qualif EnumP (x: a, p: Str): enumProp x p */



class Point<M extends ReadOnly> {
  x: number = 1;
  y: number = 2;
  constructor () {}
}

assert("x" in new Point());  // returns true
assert("y" in new Point());  // returns true
