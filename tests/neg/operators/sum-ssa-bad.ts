
/*@ sumLoop :: (number, number) => number */
function sumLoop(acc, i){
  let r = 0;
  if (0 < i){
    r = sumLoop(acc + 1, i - 1);
  }  
  return r;
}

/*@ main :: () => void */
function main(){
  let n = pos();
  let m = sumLoop(0, n);
  assert(m === n);
}
