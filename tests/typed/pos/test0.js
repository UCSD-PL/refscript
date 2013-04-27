/*@ inc :: () => int @*/
function foo(){
  var x = 0;
  var y = x + 1;
  assert (0 < y);
  return y;
}
