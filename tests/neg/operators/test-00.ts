/*@ main :: () => { void | 0 < 1 } */ 
function main(){
  var x = 0;
  var y = x - 1;
  assert (0 < y);
}
