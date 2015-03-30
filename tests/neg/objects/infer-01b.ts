/*@ qualif PLusOne(v:number, w: number): v = w + 1 */
/*@ qualif Eq5(v:number): v = 5                    */

/*@ inc :: (number) => number  */
function inc(n) {
  return n + 2;
}

/*@ readonly gobj :: # */
var gobj = {
  a: 5,
  b: "String",
  f: inc
};

/*@ foo :: () => { number | v = 6 } */
function foo () {
  var ff = gobj.f;
  return ff(gobj.a);
}
