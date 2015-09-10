/*@ glob :: { number | v > 10 } */
var glob = 20;

/*@ zog :: () => {void | true} */
function zog() {
  glob = 3;
}
