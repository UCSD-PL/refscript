/*@ glob :: { number | v > 10 } */
var glob = 12;

/*@ zog :: () => {boolean | true} */
function zog(){
  var z = true;
  glob  = z; 
  z     = 45;
  return z;
}
