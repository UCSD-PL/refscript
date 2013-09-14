
/*@ foo :: ( { v: number| v = 1 } ) => number */
function foo (n) {

  var a = [n]; 

  a[0] = 1; // This is NOT a strong update and will succeed because 
            // { number | v = 1 }  <: { number | v = n ∧ n = 1 } 

  return a[0];

}
