// This is disallowed and appear as an occurs check fail in unify

/*@ foo3 :: forall A. (A) => A  +  number */
function foo3(x) {
  var a = 1;
  if (a > 10) 
    return a;
  return x;
}

