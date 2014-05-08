
class Point {

  /*@ x :: number */
  public x : number;

  /*@ y :: number */
  public y : number;

}


class ColorPoint extends Point {
  
  /*@ c :: string */
  public c : string; 

}

class RedPoint extends ColorPoint {

  /*@ c :: { string | v = "red" } */
  public c : string = "red";

}

class Color {
  /*@ c :: string */
  public c: string;
}


// In TS casting succeeds when subtyping succeeds either way.

// /*@ a :: #RedPoint */
// var a : RedPoint = <RedPoint> { c: "red" };

