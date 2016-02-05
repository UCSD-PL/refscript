
/*@ global gobj :: (Mutable) { a: number; b: string } */
let gobj = {a: 1, b: "glorp" };

export function foo(): void {
  gobj.a = gobj.a + 1;
  return;
}

export function moo(): void{
	foo();
	let z = gobj.a;
	assert(z > -11);
	return;
}
