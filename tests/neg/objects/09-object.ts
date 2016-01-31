
/*@ qualif Gt1(v: int): v > 1 */
/*@ qualif Gt2(v: int): v > 2 */

/*@ global */
let gobj = {
	a: 1,
	b: "glorp",
};

export function foo(): void {
  gobj.a = gobj.a + 1;
  return;
}

export function moo(): void{
	foo();
	let z = gobj.a;
	assert(z > 2);
	return;
}
