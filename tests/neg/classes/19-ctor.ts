
class AA<M extends ReadOnly> {

  /*@ (Mutable) x: number */
  public x = 0;

  // Refinement CANNOT refer to mutable fields

  /*@ (Immutable) y: { number | v = this.x } */
  public y = 0;

  constructor() { }
}

let aa = new AA();
aa.x = 1;
assert(aa.x === aa.y);
