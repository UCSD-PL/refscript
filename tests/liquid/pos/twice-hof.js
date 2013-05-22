/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f, x0){
  var x1 = f(x0);
  var x1 = f(x1);
  return x1;
}

/*@ main :: ({x:int|true}) => { v:int |v >= x} */
function main(x){
  var yr = id(y);
  var xr = id(x);
  var z  = 0;
  if (yr) {
    z = 10;
  }

  function plus(a){ return a + z };

  return twice(plus, xr);
}
 


