/*@ g1 :: { number | v > 0 }*/
var g1 = 4;

/*@ g2 :: { string | true } */ 
var g2 = 2;

/*@ bar :: () => {void | true} */
function bar(){
  g1 = 7; 
  return;
}

/*@ zoo :: () => {void | true} */
function zoo(){
  bar();
  assert(g1 > 0);
}
