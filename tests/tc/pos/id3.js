/*@ id2 :: forall A. (A, A) => A */
function id2(x, y) { return x;}

/*@ main :: (number, boolean|number) => number */
function main(x, y){
  var yr = id2(y, x);
  var xr = id2(x, y);
  var z  = 0;
  var p  = crash(); 
  if (yr) {
    z = 10;
  }
  return xr + z + p;
  
}
  
