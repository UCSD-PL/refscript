
/*@ glob :: { number | v > 0 } */ 
var glob = 4;

/*@ bar :: () => {void | 0 < 1} */
function bar(){
  glob = 7; 
  return;
}

/*@ zoo :: () => {void | 0 < 1} */
function zoo(){
  bar();
  assert(glob > 10);
}
