/*@ main :: () => { void | 0 < 1 } */ 
function main(){
  let x = 0;
  let y = x - 1;
  assert (0 < y);
}
