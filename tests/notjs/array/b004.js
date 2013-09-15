
/*@ foo :: ( { v: number| v > 0} ) => number */
function foo (n) {

  var a = [];  

  if (n < 5) a[0] = 2;
  else       a[0] = 5; 

  return a[0];

}
