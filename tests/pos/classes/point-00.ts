
class Point {
  public x : number = 1;
  public y : number = 2;
}

class ColorPoint extends Point {
  public c : string = ""; 
}

class RedPoint extends ColorPoint {
  /*@ c : { string | v = "red" } */
  public c : string = "red";
}

/*@ a :: { v: { x: number; y: number } | extends_class(v, "Point") } */
var a : Point = new Point(); 

assert(a instanceof Point);

