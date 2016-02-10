
/*@ abs :: (x:number) => {v:number | v >= x} */ 
function abs(x:number) : number{
  let res = 0;
  if (x > 0) {
    res = x;
  } else {
    res = -x;
  };
  assert(res >= 0);
  return res;
}
