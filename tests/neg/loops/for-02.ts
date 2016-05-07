
/*@ qualif Ineq(v : int ): (v <= 6) */
/*@ qualif Ineq(v : int ): (v <= 7) */

/*@ loop :: () => { number | v = 7 } */
function loop() : number{

    let x = 0;

	for(x = 1; x <= 5; x ++) {
	}

	return x;
}
