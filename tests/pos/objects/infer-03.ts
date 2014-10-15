
/*@ qualif G11(v:number)            : v > -11                   */

/*@ gobj :: [#Mutable]{ a: [#Mutable] { number | v > 0 } } */
var gobj = {
	a: 1,
	b: "glorp",
};

function foo(): void {
	gobj.a = gobj.a + 1;
	return;
}

function moo(): void{
	foo();
	var z:number = gobj.a;
	assert(z > -11);
	return;
}

