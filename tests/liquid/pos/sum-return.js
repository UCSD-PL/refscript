
/*@ sumLoop :: (int, int) => int */
function sumLoop(acc, i){
  requires(0 <= i);
  ensures($result == acc + i);
  if (0 < i){
    return sumLoop(acc + 1, i - 1);
  }  
  return 0;
}

/*@ main :: () => void */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
