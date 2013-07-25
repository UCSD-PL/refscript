
/*@ foo ::  (number) => (number|boolean) */
function foo(x) {
  var r = 1;
  if (x > 0) {
    r = 1;  
  }
  else {
    r = true;
  }
  return r;
}

