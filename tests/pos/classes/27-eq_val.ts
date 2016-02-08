// This currently works

/*@ qualif CmpZ(v:number): v = 1  */

class A<M extends ReadOnly> {
  /*@ x : { number | v = 1 } */
  public x = 1;
  /*@ y : { number | v = 1 } */
  public y = 1;
  constructor() {  }
}

export function foo(): void {
  let r = new A();
  let p = r.x;
  let q = r.y;
  assert (p === q);
}
