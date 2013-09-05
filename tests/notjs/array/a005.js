/*@ foo :: () => { v: number | v = 2 } */
function foo () {
  var x = Array(2);
  // return x[0]; //should be undef
  return x.length;
}
  
