/*@ glob :: { number | v > 0 } */
var glob = 12;

/*@ bar :: () => {void | 0 < 1} */
function bar():void{
  glob = 7; 
  return;
}

