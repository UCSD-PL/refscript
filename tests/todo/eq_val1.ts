

/*@ qualif CmpZ(v:number): v = 1  */

/*@ class <M> A<M> [ extends ... ] */
class A {
  /*@ x : { number | v = 1 } */
  public x = 1;
  /*@ y : { number | x = v } */
  public y = x;
}

/*@ foo :: () => {void | true} */
function foo(){
  var r = new A();
  var p = r.x;
  //var q = r.y;
  assert (p === 1); 
}

