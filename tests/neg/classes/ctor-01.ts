

class A {
  /*@ x: [Mutable] { number | v > 0 } */
  public x: number;

  /*@ y: [Mutable] { string | (v = "a") || (v = "c") } */
  public y: string; 

  public c = "aaa"; 

  public foo() { return 1; }

  constructor() {
    if (random()) { this.y = "a"; } else { this.y = "b"; }
    if (random()) { this.x = 2; } else { this.x = 3; }
  }
}


