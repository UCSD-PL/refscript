
class Point {

  /*@ x : number */
  public x : number = 1;

  /*@ y : number */
  public y : number = 2 ;

}


class ColorPoint extends Point {
  
  /*@ c : string */
  public c : string = ""; 

}

class RedPoint extends ColorPoint {

  /*@ c : { string | v = "red" } */
  public c : string = "red";

}

/*@ a :: { v: { x: number; y: number } | instanceof(v, "Point") } */
var a : Point = new Point(); 

assert(a instanceof Point);

