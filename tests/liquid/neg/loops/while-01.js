/*@ loop :: () => { number | v = 800 } */
function loop(){
  var x = 1;
  while (x <= 5) {
    var mickey = x + 1;
    x = mickey;
  }  
  assert(0 == 1);
  return x;
}
