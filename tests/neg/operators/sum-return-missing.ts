
/*@ sumLoop :: (number, number) => number */
function sumLoop(acc, i){
  requires(0 <= i);
  var r = 0;
  if (0 < i){
    var r = sumLoop(acc + 1, i - 1);
    return r;
  }  
  // return r;
}

/*@ main :: () => void */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m === n);
}