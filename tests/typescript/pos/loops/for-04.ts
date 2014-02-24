
/*@ qualif Ineq(v : number ): (v <= 6) */

/*@ loop :: () => { number | v = 6 } */
function loop() : number{  
	var x : number = 1;
  
	for(var x : number= 1; x <= 5; x ++) {

  }

  return x;
}

