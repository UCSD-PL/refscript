
/*@ qualif G11(v:number)            : v > -11                   */

/*@ gobj :: { a: { number | v > 0 } } */
var gobj = {
	a: 1,
	b: "glorp",
};

/*@ foo :: () => void */
function foo():void {
	gobj.a = gobj.a + 1;
	return;
}

/*@ moo :: () => {void | true} */ 
function moo():void{
	foo();
	var z:number = gobj.a;
	assert(z > -11);
	return;
}

