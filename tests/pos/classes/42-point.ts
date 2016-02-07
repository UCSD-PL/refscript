
class Point {
  public x : number = 1;
  public y : number = 2;
  constructor() { }
}

class ColorPoint extends Point {
  public c : string = ""; 

  constructor() { super(); }
}

class RedPoint extends ColorPoint {
  /*@ c : { string | v = "red" } */
  public c : string = "red";
  constructor() { super(); }
}

/*@ a :: { v: { x: number; y: number } | extends_class(v, "Point") } */
var a : Point = new Point(); 

assert(a instanceof Point);

