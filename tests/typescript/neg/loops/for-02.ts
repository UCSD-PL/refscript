
/*@ qualif Ineq1(v : number ): (v <= 6) */
/*@ qualif Ineq2(v : number ): (v <= 7) */
/*@ qualif Ineq3(v : number ): (v <= 8) */

/*@ loop :: () => { number | v = 7 } */
function loop(){  
  var x = 1;
  
  for(var x = 1; x <= 5; x += 1) {
  }

  return x;
}

