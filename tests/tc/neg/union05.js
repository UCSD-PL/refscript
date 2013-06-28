
/*@ foo :: forall A B. ((number|B)) => number */
function foo(x) {
  var r = 1;
  if (x > 0) {
    r = 1;  
  }
  else {
    r = true;
    if (x < -1)  r = 1;
    else r = 2;
  }
  return 1;
}

