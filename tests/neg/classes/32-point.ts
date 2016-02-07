
class Point {
  public x : number;
  public y : number;
  constructor() {}
}

class ColorPoint extends Point {
  public c : string; 
  constructor() { super(); }
}

class RedPoint extends ColorPoint {
  /*@ c : { string | v = "red" } */
  public c : string = "red";
  constructor() { super(); }
}

/*@ a :: RedPoint<Mutable> */
var a : RedPoint = { x: 1, y: 2, c: "blue" };

