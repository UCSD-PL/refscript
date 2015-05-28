/*@ abs :: ({ x:number | 0 < 1}) => number */ 
function abs(x){
  var y = x;
  if (x > 0) {
    y = x;
  } else {
    y = 10 + x;
  };
  assert(y > 10);
  assert(y >= 100);
  return y;
}
