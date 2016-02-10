function idt<A>(x:A): A { return x;}

function apply<A,B>(f: (a: A) => B, x: A): B {
  return f(x);
}

/*@ main :: (x:number, boolean) => { v:number |v > x} */
function main(x,y){
  let yr = idt(y);
  let xr = idt(x);
  /*@ z :: number */
  let z  = 0;
  if (yr) {
    z = 10;
  }

  /*@ plus :: (number) => number */
  function plus(a){ return a + z };
  
  xr = apply(plus, xr);
  xr = apply(plus, xr);

  return xr;
}
