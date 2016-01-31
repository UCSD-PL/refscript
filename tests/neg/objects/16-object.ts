/*@ qualif PLusOne(v:number, w: number): v = w + 1 */
/*@ qualif Eq5(v:number): v = 5                    */

function inc(n: number) { return n + 1; }

/*@ readonly */
let gobj = { a: 5, b: "String", f: inc };

/*@ foo :: () => { number | v = 6 } */
function foo () {
  gobj.a = 120;
  let ff = gobj.f;
  return ff(gobj.a);
}
