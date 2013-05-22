/*@ twice :: forall A. ((A) => A, A) => A */
function twice(f, x0){
  var x1 = f(x0);
  var x1 = f(x1);
  return x1;
}

/*@ foo :: (int) => int */
function foo(x){
  var z  = 0;
  if (random() > 0) {
    z = 10;
  }
  return x + z;
}
 
/*@ main :: ({n:int|true}) => {v:int |v >= n} */
function main(x){
  return twice(foo, x);
}
