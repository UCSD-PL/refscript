
// Ha ha. Why is this safe? :)

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc, i){
  var r = acc;
  
  if (0 < i){
    r = sumLoop(acc + 1, i);
  } 
  
  return r;
}

/*@ main :: () => void */
function main(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
