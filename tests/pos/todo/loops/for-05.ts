
/*@ qualif Poo(v:number, i:number): v = i - 1 */
/*@ qualif Poo(v:number): v < 5 */

/*@ bar :: () => { number | 4 = v } */ 
function bar() : number{
	var z : number = -1;
	for (var i :number= 0; i < 5; i++) {
		z = i;
	}
	return z;
}

