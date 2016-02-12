
/*@ abs :: (x:number) => {v:number | 0 < 1} */ 
function abs(x){
  let res = 0;
  if (x > 0) {
    res = x;
  } else {
    res = -x;
  };
  assert(res >= 10);
  return res;
}
