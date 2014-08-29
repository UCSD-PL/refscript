/*@ glob :: { number | v > 0 } */
var glob :number = 4;

/*@ bar :: () => {void | true} */
function bar():void{
  glob = 7; 
  return;
}

/*@ zoo :: () => {void | true} */
function zoo():void{
  bar();
  assert(glob > 0);
}
