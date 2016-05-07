

class A<M extends ReadOnly> {

  /*@ (Immutable) x: posint */
  public x: number;

  /*@ (Immutable) y: string */
  public y: string;

  public c = "aaa";

  constructor() {
    if (random())
      this.y = "a";
    else
      this.y = "b";

    if (random())
      this.x = 2;
    else
      this.x = 3;

  }
}
