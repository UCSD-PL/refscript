/*@ qualif NonNeg(v:int): v >= 0 */

/*@ abs :: ({ x:int | true }) => int */ 
function abs(x){
  var res = 0;
  if (x > 0) {
    res = x;
  } else {
    res = -x;
  };
  assert(res >= 0);
  return res;
}
