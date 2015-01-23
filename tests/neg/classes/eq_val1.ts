// This currently works

/*@ qualif CmpZ(v:number): v = 1  */

class A {
  /*@ x : { number | v = 1 } */
  public x = 1;
  /*@ y : { number | v = 1 } */
  public y = 12;
}

/*@ foo :: () => {void | true} */
function foo(){
  var r = new A();
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}

