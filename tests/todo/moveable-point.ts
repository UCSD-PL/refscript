
/*@ qualif Tag(v:a,x:string): ttag(v) = x */

interface Point { x:number; y:number }
class MovablePoint implements Point {
  public x: number;
  public y: number;

  /*@ new (x: number, y: number) => void */
  constructor(x:number, y:number) {
    this.x = x;
    this.y = y
  }
  /*@ move : [Mutable] (dx: number, dy: number): void */
  public move(dx:number, dy:number) { this.x += dx; this.y += dy; }
}
/*@ mustBeTrue :: (x: MovablePoint<Mutable> + null) => { v: boolean | Prop(v) } */ 
function mustBeTrue(x:MovablePoint) {
  // XXX: This works:
  // return !x || <MovablePoint>x instanceof MovablePoint;

  // XXX: But not this:
  return !x || x instanceof MovablePoint;
}
