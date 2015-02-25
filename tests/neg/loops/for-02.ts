
/*@ qualif Ineq(v : number ): (v <= 6) */
/*@ qualif Ineq(v : number ): (v <= 7) */
/*@ loop :: () => { number | v = 7 } */
function loop() : number{  
  
	for(var x = 1; x <= 5; x += 1) {
	}

	return x;
}

