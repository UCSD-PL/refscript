var glob :number/*@ { number | v > 0 } */ = 4;

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
