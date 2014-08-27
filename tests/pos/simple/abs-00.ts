/*@ ab :: (number) => {res: number | 0 <= res} */
function ab(x:number):number{
  var r = x;
  if (x > 0) {
      r = x;
  } else {
      r = (0 - x); // minus(0, x);
  }
  return r;
}


/*@ minus :: (x:number, y:number) => {v:number | v = x - y} */
function minus(x, y) { 
  return x - y; 
}