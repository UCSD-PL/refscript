// This example is indicative of how rigid the system currently is 
// with respect to strong updates on arrays.


/*@ foo :: ( { v: number| v > 0} ) => number */
function foo (n) {

  var a = [n]; 

  a[0] = 1; // This is a strong update and will fail because we cannot prove that 
            // { number | v = 1 } <: { number | v > 0 ∧ v = n }
            // We do not know how n compares to 1

  return a[0];

}
