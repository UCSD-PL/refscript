/*@ glob :: { number | v > 0 } */
var glob = 12;

/*@ bar :: () => {void | true} */
function bar(){
  glob = 7; 
  return;
}

/*@ zoo :: () => {void | true} */
function zoo(){
  bar();
  assert(glob > 0);
}
