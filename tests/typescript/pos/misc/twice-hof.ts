/*@ idt :: forall A. (A) => A */
function idt(x) { return x;}

/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f, x0){
  var x1 = f(x0);
  var x1 = f(x1);
  return x1;
}

/*@ main :: (x:number, boolean) => { v:number |v >= x} */
function main(x,y){
  var yr = idt(y);
  var xr = idt(x);
  var z  = 1;
  if (yr) {
    z = 10;
  }

  assert (z > 0);

  /*@ plus :: (number) => number */
  function plus(a){ return a + z };

  return twice(plus, xr);
}
 


