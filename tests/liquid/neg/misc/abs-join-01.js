/*@ abs :: (   { } + {a:number} ) => number */ 
function abs(x){
  var res = true; //0;
  if (x > 0) {
    res = x;
  }
  //else {
  //  res = (x > 99);
  //};
  //assert(res >= 0);
  return res;
}
