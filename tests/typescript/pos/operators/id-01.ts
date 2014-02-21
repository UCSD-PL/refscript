/*@ id2 :: forall A B. (A, B) => A */
function id2(x, y) { return x;}

/*@ main :: ({x:number|true}, boolean) => {v:number|v >= x} */
function main(x, y){
  var yr = id2(y, x);
  var xr = id2(x, y);
  var z  = 0;
  if (yr) {
    z = 10;
  }
  return xr + z;
}
  
