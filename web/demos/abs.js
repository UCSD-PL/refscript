/*@ abs :: (x:int) => {v:int | v >= x} */ 
function abs(x){
  var res = true;
  if (x > 0) {
    res = x;
  } else {
    res = -x;
  }
  assert(res >= 0);
  return res;
}
