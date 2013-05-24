
/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f, x0){
  var x1 = f(x0);
  var x1 = f(x1);
  return x1;
}

/*@ foo :: (x:int) => {v:int | v >= x} */
function foo(x){
  var z  = 0;
  if (random() > 0) {
    z = 10;
  }
  var r = x + z;
  assert(r >= x);
  return r;
}
 
/*@ main :: (x:int) => {v:int |v >= x} */
function main(x){
  return twice(foo, x);
}
