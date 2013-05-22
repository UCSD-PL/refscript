/*@ twice :: forall A. ((A) => A, A) => B */
function twice(f, x0){
  var x1 = f(x0);
  var x1 = f(x1);
  return x1;
}

