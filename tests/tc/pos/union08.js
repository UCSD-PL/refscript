
/*@ foo3 :: forall A. (A) => A | int */
function foo3(x) {
  var a = 1;
  if (a > 10) 
    return a;
  return x;
}

