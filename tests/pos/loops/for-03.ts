
/*@ qualif Ineq(v : number ): (v <= 6) */

// related: while-05.js

/*@ loop :: () => { number | v > 0 } */
function loop() : number{
	let x = 1;

	for(let i = 1; i <= 5; i ++) {
		x = x + x;
	}

	return x;
}
