/*@ qualif PLusOne(v:number, w: number)     : v = w + 1                            */

/*@ inc :: (number) => number  */
function inc(n) {
  return n + 1;
}

var obj = {
  a: 5,
  b: "String",
  f: inc
}

/*@ foo :: () => { number | v = 6 } */
function foo () {
  
  var ff = obj.f;
  return ff(obj.a);

}
