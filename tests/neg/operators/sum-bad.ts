
/*@ sumLoop :: ({acc:number | true}, {i:number| 0 <= i}) => {v:number|v = acc + i} */
function sumLoop(acc, i){
  let r = 0;
  if (0 < i){
    r = sumLoop(acc + 1, i);
  } else {
    r = acc;
  }
  return r;
}

/*@ main :: () => void */
function main(){
  let n = pos();
  let m = sumLoop(0, n);
  assert(m === n);
}
