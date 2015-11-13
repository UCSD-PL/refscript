/*@ g1 :: number */
/*@ g2 :: string */ 
var g1 : number = 4,
    g2 : string = "AAA";

/*@ bar :: () => {void | 0 < 1} */
function bar():void{
  g1 = 7; 
  return;
}

/*@ zoo :: () => {void | 0 < 1} */
function zoo():void{
  bar();
  assert(g1 > 0);
}
