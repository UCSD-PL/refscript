
interface Point {
  x: number;
  y: number;
}

interface ColorPoint extends Point {
  c: string; 
}

interface RedPoint extends ColorPoint {
  /*@ c : { string | v = "red" } */
  c: string;
}

/*@ a :: RedPoint<Mutable> */
var a : RedPoint = { x: 1, y: 2, c: "blue" };

