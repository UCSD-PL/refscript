/*@ glob :: { number | v > 15 } */
var glob = 20;

/*@ zog :: () => {void | true} */
function zog(){
  glob = 3;
}
