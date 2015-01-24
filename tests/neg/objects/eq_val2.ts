
/*@ predicate EqXY v = keyVal(v,"x") ~~ keyVal(v, "y") */
/*@ alias Point = {x:number; y: number} */
/*@ alias EqPoint = {v:Point | EqXY v} */

/*@ foo :: () => EqPoint */
function foo() {

  return { x: random(), y: random() }; 

}

/*@ bar :: () => {void | true} */
function bar() {
  var r = foo();
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}


