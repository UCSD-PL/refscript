

class A {
  /*@ x: [Immutable] { number | v > 0 } */
  public x: number;

  /*@ y: [Immutable] string */
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


