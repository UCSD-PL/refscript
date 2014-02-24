
/*@ foo :: ([number]) => { number | true } */
function foo(a) {
  
  return a[0];      // INCORRECTLY marked "SAFE" because 
                    // special case-tuple check calls `getIdx` 
                    // which IGNORES length

  //var x = 0;
  // return a[x];   // CORRECTLY marked "UNSAFE"
}
