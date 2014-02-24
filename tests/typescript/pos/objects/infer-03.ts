var gobj = {
	a: 5,
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
	assert(z > 0);
	return;
}

