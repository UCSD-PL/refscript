/*@ glob :: { number | v > 10 } */
var glob = 20;

/*@ zog :: () => {void | 0 < 1} */
function zog(){
  glob = 3;
}
