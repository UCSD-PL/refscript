/*@ g1 :: { number | v > 0 }*/
var g1 = 4;

/*@ g2 :: { string | 0 < 1 } */ 
var g2 = 2;

/*@ bar :: () => {void | 0 < 1} */
function bar(){
  g1 = 7; 
  return;
}

/*@ zoo :: () => {void | 0 < 1} */
function zoo(){
  bar();
  assert(g1 > 0);
}
