/*@ qualif Ineq1(v : number ): (v <= 5) */

/*@ foo :: () => { number | v = 5 } */ 
function foo() {
  var x = 0;
  var i = 1; 
  while (i < 5) {
     x = i;
     i = i + 1; 
  }
  return i;
}
