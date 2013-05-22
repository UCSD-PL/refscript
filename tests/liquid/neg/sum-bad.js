
/*@ sumLoop :: ({acc:int | true}, {i:int| 0 <= i}) => {v:int|v = acc + i} */
function sumLoop(acc, i){
  var r = 0;
  if (0 < i){
    r = sumLoop(acc + 1, i);
  } else {
    r = acc;
  }
  return r;
}

/*@ main :: () => void */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
