
/*@ foo :: ( { v: number| v > 0} ) => number + string */
function foo (n) {

  var a = [];  

  if (n < 5) a[0] = 2;
  else       a[0] = "2"; 

  return a[0];

}
