
/*@ qualif Ineq3(v : number ): (5 <= v) */

/*@ foo :: (x: [#Mutable]{ a: [#Mutable] number }) => { number | 0 < 1 } */ 

function foo(x) : number {

	for (var i : number = 0; i < 5; i ++) {
		x.a = i;  
	}
	
	return x.a;

}
