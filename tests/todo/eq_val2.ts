

/* qualif CmpZ(v:number): v = 1 */

/*@ predicate EqXY v = keyVal(v,"x") = keyVal(v, "y") */
/*@ alias Point = {x:number; y: number} */
/*@ alias EqPoint = {v:Point | EqXY v} */
/*@ foo :: () => EqPoint */

function foo() {

  var a = random();

  /*@ b :: EqPoint */
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


