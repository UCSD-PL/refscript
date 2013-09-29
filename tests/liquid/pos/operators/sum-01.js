
/*@ sumLoop :: (number, number) => number */
function sumLoop(acc, i){
  if (0 < i){
    return sumLoop(acc + 1, i - 1);
  }  
  return acc;
}

/*@ main :: () => void */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
