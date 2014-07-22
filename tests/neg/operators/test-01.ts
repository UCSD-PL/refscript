/*@ main :: () => { void | true } */
function main(){
  var y = 0;
  var x = random();
  y = x + y;
  assert (0 <= y);
}


