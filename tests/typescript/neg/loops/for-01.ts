
/*@ qualif Ineq(v : number ): (v <= 6) */
/*@ qualif Ineq(v : number ): (v <= 7) */
/*@ qualif Ineq(v : number ): (v <= 8) */

/*@ loop :: () => { number | v = 7 } */
function loop(){  
  
  for(var x = 1; x <= 5; x = x + 1) {
    
  }
  return x;
}

