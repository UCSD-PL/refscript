
/*@ foo :: ( { w: number| w > 0} ) => { number | v > 0 } */ 
function foo (n) {

  var a = []; 

  // a will be instantiated to an array of { number | v > 0 } 
  a[0] = n; 

  return a[0];

}
