/*@ id :: forall A. (A) => A */
function id(x) { return x;}


/*@ idbool :: (bool) => bool */
function idbool(x) { return id(x); }

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
 

