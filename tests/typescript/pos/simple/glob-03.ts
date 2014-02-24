/*@ g1 :: { number | v > 0 }*/
/*@ g2 :: string */ 
var g1 : number = 4,
    g2 : string = "AAA";

/*@ bar :: () => {void | true} */
function bar():void{
  g1 = 7; 
  return;
}

/*@ zoo :: () => {void | true} */
function zoo():void{
  bar();
  assert(g1 > 0);
}
