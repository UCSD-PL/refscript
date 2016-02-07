// This currently works

/*@ qualif CmpZ(v:number): v = 1  */

class A {
  /*@ x : { number | v = 1 } */
  public x = 1;
  /*@ y : { number | v = 1 } */
  public y = 12;

  constructor() {  } 
}

/*@ foo :: () => {void | 0 < 1} */
function foo(){
  var r = new A();
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}

