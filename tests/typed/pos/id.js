/*@ id :: forall A. (A) => A */
function id(x) { return x;}

/*@ main :: (int, bool) => int */
function main(x, y){
  var yr = id(y);
  var xr = id(x);
  var z  = 0;
  if (yr) {
    z = 10;
  }
  return xr + z;
}
  
