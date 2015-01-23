

/* qualif CmpZ(v:number): v = 1 */

function foo() {
  var a = random();
  
  /*@ b :: { vv: { x: number; y: number } | (keyVal(vv,"x") = keyVal(vv, "y")) } */
  var b = { x: a, y: a } ; 
  return b;
}

/*@ bar :: () => {void | true} */
function bar() {
  var r = foo();
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}


