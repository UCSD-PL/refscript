
class A {
  // Try changing Immutable to Mutable - it will fail because
  // the refinement on y can't refer to a mutable field
  /*@ x: [Immutable] number */
  public x: number;

  /*@ y : [Mutable] { number | v = this.x } */
  public y: number;

  constructor(a: number) {
    this.x = a;
    this.y = a;
  }

}

/*@ foo :: () => {void | 0 < 1} */
function foo(){
  var r = new A(29);
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}

