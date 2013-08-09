// Error rigid unify


/*@ foo3 :: forall A. (A) => A  */
function foo3(x) {
  var a = 1;
  if (a > 10) 
    return a;
  return x;
}

