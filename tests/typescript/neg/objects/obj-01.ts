
/*@ inc :: (n: number) => number */
function inc(n) {
  return n + 1;
}

var obj = {
  a: 5,
  b: "String",
  f: inc
};

/*@ foo :: () => { v: number | v = 7 } */
function foo () {
  
  var ff = obj.f;
  return ff(obj.a);

}
