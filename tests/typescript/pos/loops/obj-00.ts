
/*@ qualif Ineq3(v : number ): (5 <= v) */

/*@ foo :: (x: { a: number }) => { number | true } */ 

function foo(x) : number {

	for (var i : number = 0; i < 5; i ++) {
		x.a = i;  
	}
	
	return x.a;

}
