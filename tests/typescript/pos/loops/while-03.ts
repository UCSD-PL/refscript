/*@ qualif Ineq1(v : number ): (v <= 5) */

/*@ foo :: () => { number | v = 5 } */ 
function foo() : number {
	var x : number = 0;
	var i : number= 1; 
	while (i < 5) {
		x = i;
		i = i + 1; 
	}
	return i;
}
