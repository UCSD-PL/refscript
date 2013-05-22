/*@ id :: forall A. (A) => A */
function id(x) { return x;}


/*@ idbool :: (boolean) => boolean */
function idbool(x) { return id(x); }

/*@ main :: ({x:int|true}, boolean) => { v:int |v >= x} */
function main(x, y){
  var yr = id(y);
  var xr = id(x);
  var z  = 0;
  if (yr) {
    z = 10;
  }
  return xr + z;
}
