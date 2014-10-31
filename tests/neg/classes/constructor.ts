
class Point {
  public x = 1;
  public y = 2
}

var obj = { x: 2, y: 3 };

var p = <Point> obj;

assert(p instanceof Point);

