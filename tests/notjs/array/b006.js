
/*@ foo :: ( { v: number| v = 1 } ) => number */
function foo (n) {

  var a = 1;

  if (n > 0) 
  a = 2;
  else {
    a = 5;
    a = 4;
  }

  return a;

}
