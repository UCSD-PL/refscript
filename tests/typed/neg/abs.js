/*@ abs :: ({ x:int | true }) => int */ 
function abs(x){
  var y = x;
  if (x > 0) {
    y = x;
  } else {
    y = -x;
  };
  assert(y > 10);
  assert(y >= 100);
  return y;
}
