
class A {
  
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

