var glob /*@ { number | v > 10 } */ = 12;

/*@ zog :: () => {boolean | true} */
function zog(){
  var z = true;
  glob  = z; 
  z     = 45;
  return z;
}
