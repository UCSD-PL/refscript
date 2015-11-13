/*@ glob :: { number | v > 0 } */
var glob :number = 4;

/*@ bar :: () => {void | 0 < 1} */
function bar():void{
  glob = 7; 
  return;
}

/*@ zoo :: () => {void | 0 < 1} */
function zoo():void{
  bar();
  assert(glob > 0);
}
