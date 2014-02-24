/*@ qualif PLusOne(v:number, w: number): v = w + 1 */
/*@ qualif Eq5(v:number): v = 5                    */

/*@ inc :: (number) => number  */
function inc(n:number) :number{
  return n + 1;
}

var gobj = {
  a: 5,
  b: "String",
  f: inc
};

/*@ foo :: () => { number | v = 6 } */
function foo ():number {
 
	// gobj.a = 120;

	var ff :(number)=>number = gobj.f;
	return ff(gobj.a);

}
