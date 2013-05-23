/*@ idt :: forall A. (A) => A */
function idt(x) { return x;}

/*@ apply :: forall A B. ((A) => B, A) => B */
function apply(f, x0){
  var x1 = f(x0);
  return x1;
}

/*@ main :: (x:int, boolean) => { v:int |v > x} */
function main(x,y){
  var yr = idt(y);
  var xr = idt(x);
  var z  = 0;
  if (yr) {
    z = 10;
  }

  /*@ plus :: (int) => int */
  function plus(a){ return a + z };
  
  xr = apply(plus, xr);
  xr = apply(plus, xr);

  return xr;
}
 


