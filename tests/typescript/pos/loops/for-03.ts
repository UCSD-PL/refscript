
/*@ qualif Ineq(v : number ): (v <= 6) */

// related: while-05.js

/*@ loop :: () => { number | v > 0 } */
function loop(){  
  var x = 1;
  
  for(var i = 1; i <= 5; i += 1) {
    x = x + x;
  }

  return x;
}

