
/* qualif Eq1(v : number, w: number): (v = w ) */
/*@ qualif Eq2(v : number, w: number): (v = w - 1) */
/*@ qualif Eq3(v : number, w: number): (v = w + 1) */
/* qualif Ineq1(v : number ): (v <= 5) */
/*@ qualif Ineq2(v : number ): (v < 5) */
/* qualif Ineq3(v : number ): (4 <= v) */
/* qualif EEq1(v : number ): (v = 5) */
/* qualif EEq2(v : number ): (v = 4) */

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
